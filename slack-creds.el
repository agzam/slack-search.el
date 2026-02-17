;;; slack-creds.el --- Extract Slack credentials from local app data -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 17, 2026
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/agzam/slack-search
;; Package-Requires: ((emacs "30.2"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extract Slack API tokens and session cookies directly from the Slack
;; desktop app's local data files (LevelDB for tokens, Cookies SQLite
;; for the encrypted `d' cookie).  Cache them in a GPG-encrypted file
;; in netrc format for use with `auth-source'.
;;
;; Currently macOS only.  The cookie decryption relies on:
;; - `security' (Keychain access)
;; - `openssl' (PBKDF2 key derivation + AES-128-CBC decryption)
;; - `sqlite3' (reading the Cookies database)
;;
;;; Code:

(require 'auth-source)

;;; Customizable Variables

(defgroup slack-creds nil
  "Slack credential extraction and caching."
  :group 'tools
  :prefix "slack-creds-")

(defcustom slack-creds-gpg-file
  (expand-file-name ".slack-creds.gpg" user-emacs-directory)
  "GPG-encrypted file to cache credentials in netrc format."
  :type 'file
  :group 'slack-creds)

(defcustom slack-creds-slack-data-dir
  (pcase system-type
    ('darwin (expand-file-name "~/Library/Application Support/Slack/")))
  "Path to the Slack desktop app's data directory."
  :type 'directory
  :group 'slack-creds)

(defcustom slack-creds-keychain-service "Slack Safe Storage"
  "Keychain service name for the Slack cookie encryption key."
  :type 'string
  :group 'slack-creds)

;;; Token extraction (LevelDB)

(defun slack-creds--extract-tokens ()
  "Extract xoxc tokens from Slack's LevelDB storage.
Returns a list of token strings."
  (let* ((ldb-dir (expand-file-name "Local Storage/leveldb/" slack-creds-slack-data-dir))
         (cmd (format "strings %s*.ldb %s*.log 2>/dev/null | grep -oE 'xoxc-[A-Za-z0-9_-]+' | sort -u"
                      (shell-quote-argument ldb-dir)
                      (shell-quote-argument ldb-dir)))
         (output (string-trim (shell-command-to-string cmd))))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

;;; Cookie decryption (Cookies SQLite + Keychain + OpenSSL)

(defun slack-creds--get-keychain-password ()
  "Get the Slack Safe Storage password from macOS Keychain."
  (let ((output (string-trim
                 (shell-command-to-string
                  (format "security find-generic-password -s %s -w 2>/dev/null"
                          (shell-quote-argument slack-creds-keychain-service))))))
    (if (string-empty-p output)
        (error "Could not retrieve Slack keychain password")
      output)))

(defun slack-creds--decrypt-cookie ()
  "Decrypt the Slack `d' cookie from the Cookies SQLite database.
Returns the cookie value string or nil."
  (let* ((cookies-db (expand-file-name "Cookies" slack-creds-slack-data-dir))
         (tmp-enc (make-temp-file "slack-cookie-" nil ".bin"))
         (tmp-dec (make-temp-file "slack-cookie-dec-" nil ".bin")))
    (unwind-protect
        (progn
          ;; Extract encrypted blob, strip v10 prefix (3 bytes)
          (shell-command-to-string
           (format "sqlite3 %s \"SELECT writefile('%s', substr(encrypted_value, 4)) FROM cookies WHERE name='d' LIMIT 1;\""
                   (shell-quote-argument cookies-db) tmp-enc))

          (when (and (file-exists-p tmp-enc)
                     (> (file-attribute-size (file-attributes tmp-enc)) 0))
            (let* ((pass (slack-creds--get-keychain-password))
                   ;; Derive AES key: PBKDF2(password, salt='saltysalt', iter=1003, SHA1, keylen=16)
                   (salt-hex "73616c747973616c74") ; "saltysalt" in hex
                   (key-hex (string-trim
                             (shell-command-to-string
                              (format "openssl kdf -keylen 16 -kdfopt digest:SHA1 -kdfopt 'pass:%s' -kdfopt hexsalt:%s -kdfopt iter:1003 -binary PBKDF2 | xxd -p"
                                      pass salt-hex))))
                   (iv-hex "20202020202020202020202020202020"))

              ;; Decrypt
              (shell-command-to-string
               (format "openssl enc -aes-128-cbc -d -K %s -iv %s -nopad -in %s -out %s 2>/dev/null"
                       key-hex iv-hex
                       (shell-quote-argument tmp-enc)
                       (shell-quote-argument tmp-dec)))

              ;; Skip 32-byte domain hash prefix, strip PKCS7 padding
              (let ((raw (string-trim
                          (shell-command-to-string
                           (format "dd if=%s bs=1 skip=32 2>/dev/null | perl -pe 's/[\\x01-\\x10]+$//'"
                                   (shell-quote-argument tmp-dec))))))
                (when (string-prefix-p "xoxd-" raw)
                  raw)))))
      ;; Cleanup
      (delete-file tmp-enc)
      (when (file-exists-p tmp-dec)
        (delete-file tmp-dec)))))

;;; Workspace identification

(defun slack-creds--identify-workspace (token cookie)
  "Call auth.test to identify which workspace TOKEN belongs to.
COOKIE is the decrypted `d' cookie.  Returns an alist with
team, team_id, user, url or nil on failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" token))
            ("Cookie" . ,(format "d=%s;" cookie))
            ("Content-Type" . "application/json")))
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (buf (url-retrieve-synchronously "https://slack.com/api/auth.test" t nil 10)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (forward-line 1)
              (let* ((json-object-type 'alist)
                     (json-key-type 'symbol)
                     (resp (ignore-errors (json-read))))
                (when (eq (alist-get 'ok resp) t)
                  resp))))
        (kill-buffer buf)))))

;;; GPG file management

(defun slack-creds--read-gpg-file ()
  "Read the current contents of the credentials GPG file."
  (when (file-exists-p slack-creds-gpg-file)
    (with-temp-buffer
      (insert-file-contents slack-creds-gpg-file)
      (buffer-string))))

(defun slack-creds--update-gpg-entry (contents host login password)
  "Update or add a netrc entry in CONTENTS for HOST, LOGIN with PASSWORD.
Returns the updated string."
  (let* ((lines (split-string (or contents "") "\n" nil))
         (pattern (format "machine %s login %s " host login))
         (new-line (format "machine %s login %s password %s" host login password))
         (found nil)
         (updated (mapcar (lambda (line)
                            (if (string-prefix-p pattern line)
                                (progn (setq found t) new-line)
                              line))
                          lines)))
    (if found
        (string-join updated "\n")
      (let ((result (string-trim (or contents ""))))
        (if (string-empty-p result)
            new-line
          (concat result "\n" new-line))))))

(defun slack-creds--save-to-gpg (entries)
  "Save credential ENTRIES to the GPG file.
ENTRIES is a list of (host token cookie) triples."
  (let ((contents (slack-creds--read-gpg-file)))
    (dolist (entry entries)
      (let ((host (nth 0 entry))
            (token (nth 1 entry))
            (cookie (nth 2 entry)))
        (setq contents (slack-creds--update-gpg-entry contents host "token" token))
        (setq contents (slack-creds--update-gpg-entry contents host "cookie" cookie))))
    ;; Write back
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file slack-creds-gpg-file
        (insert contents)
        (unless (string-suffix-p "\n" contents)
          (insert "\n"))))
    (message "Slack credentials saved to %s" slack-creds-gpg-file)))

;;; Main entry point

;;;###autoload
(defun slack-creds-refresh ()
  "Extract Slack credentials from the local app and cache them.
Reads tokens from LevelDB, decrypts the session cookie, identifies
workspaces via auth.test, and saves to the GPG credentials file."
  (interactive)
  (message "Extracting Slack credentials...")
  (let ((tokens (slack-creds--extract-tokens))
        (cookie (slack-creds--decrypt-cookie))
        (entries '()))
    (unless tokens
      (error "No tokens found in Slack's LevelDB"))
    (unless cookie
      (error "Could not decrypt the Slack session cookie"))
    (message "Found %d token(s), cookie decrypted. Identifying workspaces..."
             (length tokens))
    (dolist (token tokens)
      (let ((ws (slack-creds--identify-workspace token cookie)))
        (if ws
            (let* ((url (alist-get 'url ws))
                   ;; url is like "https://qlikdev.slack.com/"
                   (host (and (string-match "https://\\([^/]+\\)" url)
                              (match-string 1 url))))
              (push (list host token cookie) entries)
              (message "  ✓ %s (%s)" host (alist-get 'user ws)))
          (message "  ✗ token %s...%s - invalid or expired"
                   (substring token 0 15)
                   (substring token -8)))))
    (if entries
        (progn
          (slack-creds--save-to-gpg entries)
          (message "Done. %d workspace(s) updated." (length entries)))
      (error "No valid credentials found"))))

;;;###autoload
(defun slack-creds-get (host kind)
  "Get cached credential for HOST (e.g. \"qlikdev.slack.com\").
KIND is either \"token\" or \"cookie\".
Reads from the GPG credentials file via `auth-source'."
  (let* ((auth-sources (list slack-creds-gpg-file))
         (auth-source-cache-expiry nil) ; don't use stale cache
         (found (car (auth-source-search :host host :user kind :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(provide 'slack-creds)
;;; slack-creds.el ends here
