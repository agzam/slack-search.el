;;; slack-thread.el --- Capture Slack threads -*- lexical-binding: t; -*-
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
;; Fetch and display Slack messages and threads given a Slack URL.
;; Works with `slack-creds' for workspace-aware authentication.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'org)
(require 'slack-mrkdwn)
(require 'slack-creds)

;;; Customizable Variables

(defgroup slack-thread nil
  "Fetch and display Slack threads."
  :group 'tools
  :prefix "slack-thread-")

(defcustom slack-thread-buffer-name "*Slack Thread*"
  "Name of the buffer to display captured threads."
  :type 'string
  :group 'slack-thread)

;;; URL Parsing

(defconst slack-thread--url-regexp
  (rx "https://" (group (+ (not "/"))) ".slack.com"
      "/archives/" (group (+ (not "/")))
      "/p" (group (+ digit))
      (* anything))
  "Regexp matching a Slack message URL.
Groups: 1=workspace, 2=channel-id, 3=raw-timestamp.")

(defun slack-thread--parse-url (url)
  "Parse a Slack message URL into its components.
Returns a plist (:workspace :channel-id :ts :thread-ts) or nil."
  (when (and url (string-match slack-thread--url-regexp url))
    (let* ((workspace (match-string 1 url))
           (channel-id (match-string 2 url))
           (raw-ts (match-string 3 url))
           ;; Convert p1771363006380029 -> 1771363006.380029
           (ts (concat (substring raw-ts 0 -6) "." (substring raw-ts -6)))
           ;; Check for thread_ts query param
           (thread-ts (when (string-match "thread_ts=\\([0-9.]+\\)" url)
                        (match-string 1 url))))
      (list :workspace workspace
            :channel-id channel-id
            :ts ts
            :thread-ts thread-ts))))

(defun slack-thread--detect-url ()
  "Detect a Slack URL from context.
Checks: thing-at-point, then latest kill-ring entry."
  (or (when-let* ((url (thing-at-point 'url t)))
        (when (string-match-p slack-thread--url-regexp url)
          url))
      (when-let* ((clip (ignore-errors (current-kill 0 t))))
        (when (string-match-p slack-thread--url-regexp clip)
          clip))))

;;; API Requests

(defun slack-thread--api-request (host endpoint params)
  "Make a synchronous Slack API request.
HOST is the workspace domain (e.g. \"qlikdev.slack.com\").
ENDPOINT is the API method (e.g. \"conversations.replies\").
PARAMS is an alist of query parameters.
Returns parsed JSON response or nil."
  (let* ((token (slack-creds-get host "token"))
         (cookie (slack-creds-get host "cookie"))
         (_ (unless token (error "No credentials for %s. Is this workspace logged in to the Slack app?" host)))
         (_ (unless cookie (error "No cookie for %s. Is this workspace logged in to the Slack app?" host)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" token))
            ("Cookie" . ,(format "d=%s;" cookie))
            ("Content-Type" . "application/json")))
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (query-params (url-build-query-string params))
         (url (format "https://slack.com/api/%s?%s" endpoint query-params))
         (buf (url-retrieve-synchronously url t nil 15)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (forward-line 1)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'symbol))
                (json-read))))
        (kill-buffer buf)))))

(defun slack-thread--fetch-thread (host channel-id ts)
  "Fetch a thread from Slack.
Uses conversations.replies with TS as the thread parent.
Returns the list of messages or nil."
  (let* ((resp (slack-thread--api-request
                host "conversations.replies"
                `((channel ,channel-id)
                  (ts ,ts)
                  (limit "200")))))
    (if (eq (alist-get 'ok resp) t)
        (alist-get 'messages resp)
      (message "Slack API error: %s" (alist-get 'error resp))
      nil)))

(defun slack-thread--fetch-single-message (host channel-id ts)
  "Fetch a single message from Slack.
Uses conversations.history with TS.
Returns a list containing the single message or nil."
  (let* ((resp (slack-thread--api-request
                host "conversations.history"
                `((channel ,channel-id)
                  (latest ,ts)
                  (inclusive "true")
                  (limit "1")))))
    (if (eq (alist-get 'ok resp) t)
        (alist-get 'messages resp)
      (message "Slack API error: %s" (alist-get 'error resp))
      nil)))

;;; User Resolution

(defvar slack-thread--user-cache (make-hash-table :test 'equal)
  "Cache of user-id -> display-name, keyed as \"host:user-id\".")

(defun slack-thread--resolve-user (host user-id)
  "Resolve USER-ID to a display name for workspace HOST.
Results are cached in `slack-thread--user-cache'."
  (let ((cache-key (format "%s:%s" host user-id)))
    (or (gethash cache-key slack-thread--user-cache)
        (let* ((resp (slack-thread--api-request
                      host "users.info"
                      `((user ,user-id))))
               (user (alist-get 'user resp))
               (profile (alist-get 'profile user))
               (name (or (alist-get 'display_name profile)
                         (alist-get 'real_name user)
                         (alist-get 'name user)
                         user-id)))
          (puthash cache-key name slack-thread--user-cache)
          name))))

(defun slack-thread--resolve-user-mentions (host text)
  "Replace <@USER_ID> mentions in TEXT with display names for HOST."
  (if (and text (string-match-p "<@U[A-Z0-9]+>" text))
      (replace-regexp-in-string
       "<@\\(U[A-Z0-9]+\\)>"
       (lambda (match)
         (let* ((uid (match-string 1 match))
                (name (slack-thread--resolve-user host uid)))
           (format "@%s" name)))
       text)
    text))

;;; Display

(defun slack-thread--format-timestamp (ts)
  "Format a Slack timestamp TS into a human-readable string."
  (when (and ts (stringp ts))
    (ignore-errors
      (format-time-string "%Y-%m-%d %H:%M"
                          (seconds-to-time (string-to-number ts))))))

(defun slack-thread--insert-message (msg host workspace channel-id level)
  "Insert a single message MSG into the current buffer.
HOST is the workspace domain.  WORKSPACE is the short name.
CHANNEL-ID is the channel.  LEVEL is the org heading level (1 or 2)."
  (let* ((user-id (alist-get 'user msg))
         (author (if user-id
                     (slack-thread--resolve-user host user-id)
                   "Unknown"))
         (ts (alist-get 'ts msg))
         (timestamp (slack-thread--format-timestamp ts))
         (text (alist-get 'text msg))
         (text (slack-thread--resolve-user-mentions host text))
         (text (if text (slack-mrkdwn-to-org text) ""))
         (files (alist-get 'files msg))
         (reactions (alist-get 'reactions msg))
         (stars (make-string level ?*))
         ;; Build permalink
         (msg-ts (replace-regexp-in-string "\\." "" ts))
         (permalink (format "slack://%s.slack.com/archives/%s/p%s"
                            workspace channel-id msg-ts)))
    (insert (format "%s %s | [[%s][%s]]\n" stars author permalink timestamp))
    (unless (string-empty-p text)
      (insert text "\n"))
    ;; Files
    (when files
      (dolist (file files)
        (let ((name (alist-get 'name file))
              (url (alist-get 'url_private file))
              (ptype (alist-get 'pretty_type file)))
          (insert (format "- [[%s][%s]]%s\n"
                          (or url "") (or name "file")
                          (if ptype (format " (%s)" ptype) ""))))))
    ;; Reactions
    (when reactions
      (insert (mapconcat
               (lambda (r)
                 (format ":%s: %d" (alist-get 'name r) (alist-get 'count r)))
               reactions "  ")
              "\n"))
    (insert "\n")))

(defun slack-thread--display (messages host workspace channel-id url)
  "Display MESSAGES in an org buffer.
HOST is the full domain.  WORKSPACE is the short name.
CHANNEL-ID and URL are for context."
  (let ((buf (get-buffer-create slack-thread-buffer-name))
        (parent (car messages))
        (replies (cdr messages)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Slack Thread\n"))
        (insert (format "#+SOURCE: %s\n" url))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        ;; Parent message
        (slack-thread--insert-message parent host workspace channel-id 1)
        ;; Replies
        (dolist (reply replies)
          (slack-thread--insert-message reply host workspace channel-id 2)))
      (unless (eq major-mode 'slack-thread-mode)
        (slack-thread-mode))
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;; Major Mode

(defvar slack-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `slack-thread-mode'.")

(define-derived-mode slack-thread-mode org-mode "Slack-Thread"
  "Major mode for displaying a captured Slack thread.
Derived from `org-mode'.
\\{slack-thread-mode-map}"
  (setq buffer-read-only t)
  ;; Blockquote highlighting
  (font-lock-add-keywords nil slack-mrkdwn-font-lock-keywords))

;;; Interactive Commands

;;;###autoload
(defun slack-thread-capture (&optional url)
  "Capture a Slack message or thread and display it in an org buffer.

URL resolution order:
1. Explicit URL argument
2. URL at point
3. Latest kill-ring entry matching a Slack URL
4. Error

If the message is a thread parent, the full thread is fetched.
If it's a standalone message, just that message is shown."
  (interactive)
  (let* ((url (or url
                  (slack-thread--detect-url)
                  (user-error "No Slack URL found at point or in kill-ring")))
         (parsed (slack-thread--parse-url url))
         (_ (unless parsed (user-error "Could not parse Slack URL: %s" url)))
         (workspace (plist-get parsed :workspace))
         (channel-id (plist-get parsed :channel-id))
         (ts (plist-get parsed :ts))
         (thread-ts (plist-get parsed :thread-ts))
         (host (format "%s.slack.com" workspace))
         ;; If URL has thread_ts, use that as the parent ts
         (parent-ts (or thread-ts ts)))
    (message "Fetching thread from %s..." host)
    (let ((messages (slack-thread--fetch-thread host channel-id parent-ts)))
      (if messages
          (slack-thread--display messages host workspace channel-id url)
        ;; Fallback: try as single message
        (let ((single (slack-thread--fetch-single-message host channel-id ts)))
          (if single
              (slack-thread--display single host workspace channel-id url)
            (user-error "Could not fetch message from %s" url)))))))

(provide 'slack-thread)
;;; slack-thread.el ends here
