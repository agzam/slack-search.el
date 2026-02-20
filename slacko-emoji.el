;;; slacko-emoji.el --- Emoji overlay support for slacko -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2025
;; Keywords: tools
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2") (emojify "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Buffer-local minor mode that overlays Slack-style shortcodes like :smile:
;; with their Unicode emoji equivalents (e.g. 😄), using the `emojify'
;; package's shortcode-to-unicode mapping.
;;
;; Custom workspace emojis (uploaded images) are also supported.  These
;; are fetched via Slack's `emoji.list' API, cached to disk, and rendered
;; as inline images at emoji scale.
;;
;; The underlying buffer text is never mutated - overlays with `display'
;; properties are used instead.  When the cursor is on an emoji overlay,
;; the original shortcode is revealed (org-appear style).
;;
;;; Code:

(require 'emojify)
(require 'url)
(require 'url-vars)

(defgroup slacko-emoji nil
  "Emoji rendering for slacko buffers."
  :group 'slacko
  :prefix "slacko-emoji-")

(defvar-local slacko-emoji--revealed-overlays nil
  "List of overlays currently revealed (showing shortcode text).")

(defvar-local slacko-emoji--buffer-host nil
  "The Slack workspace host for the current buffer.
Set by the rendering pipeline so the emoji resolver knows which
workspace's custom emojis to use.")

(defface slacko-emoji-count
  '((t :height 0.7 :inherit default))
  "Face for reaction count superscripts next to emoji."
  :group 'slacko-emoji)

(defcustom slacko-emoji-cache-directory
  (expand-file-name "slacko-emoji" (or (bound-and-true-p doom-cache-dir)
                                       user-emacs-directory))
  "Directory for caching downloaded workspace emoji images."
  :type 'directory
  :group 'slacko-emoji)

;;; Workspace emoji cache

(defvar slacko-emoji--workspace-emojis (make-hash-table :test 'equal)
  "Cache of workspace custom emojis.
Keys are HOST strings, values are hash tables mapping
shortcode names (without colons) to either an image URL string
or an \"alias:NAME\" string.")

(defvar slacko-emoji--workspace-fetched (make-hash-table :test 'equal)
  "Tracks which workspace hosts have had their emoji.list fetched this session.")

(defun slacko-emoji--fetch-workspace-emojis (host)
  "Fetch custom emoji list for HOST via the emoji.list API.
Populates `slacko-emoji--workspace-emojis' for HOST.
Only fetches once per session per host."
  (unless (gethash host slacko-emoji--workspace-fetched)
    (condition-case err
        (when-let* ((resp (progn
                            (require 'slacko-creds)
                            (slacko-creds-api-request host "emoji.list" nil)))
                    (_ (eq (alist-get 'ok resp) t))
                    (emoji-alist (alist-get 'emoji resp)))
          (let ((table (make-hash-table :test 'equal :size (length emoji-alist))))
            (dolist (pair emoji-alist)
              (puthash (symbol-name (car pair)) (cdr pair) table))
            (puthash host table slacko-emoji--workspace-emojis))
          (puthash host t slacko-emoji--workspace-fetched)
          (message "Fetched %d custom emojis for %s"
                   (hash-table-count (gethash host slacko-emoji--workspace-emojis))
                   host))
      (error
       (puthash host t slacko-emoji--workspace-fetched)
       (message "Failed to fetch workspace emojis for %s: %s" host err)))))

(defun slacko-emoji--resolve-alias (name host &optional depth)
  "Resolve emoji NAME for HOST, following aliases up to DEPTH levels.
Returns the final URL string or nil."
  (let ((depth (or depth 0))
        (table (gethash host slacko-emoji--workspace-emojis)))
    (when (and table (< depth 5))
      (let ((value (gethash name table)))
        (cond
         ((null value) nil)
         ((string-prefix-p "alias:" value)
          (slacko-emoji--resolve-alias
           (substring value 6) host (1+ depth)))
         (t value))))))

;;; Emoji image disk cache

(defun slacko-emoji--cache-dir-for-host (host)
  "Return the emoji cache directory for HOST, creating it if needed."
  (let ((dir (expand-file-name host slacko-emoji-cache-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun slacko-emoji--cached-image-path (host name)
  "Return the disk cache path for emoji NAME on HOST."
  (expand-file-name (concat name ".png")
                    (slacko-emoji--cache-dir-for-host host)))

(defun slacko-emoji--download-emoji-image (url host name)
  "Download emoji image at URL for HOST/NAME, caching to disk.
Returns the file path on success, nil on failure."
  (let ((cache-path (slacko-emoji--cached-image-path host name)))
    (if (file-exists-p cache-path)
        cache-path
      (condition-case nil
          (let* ((url-request-method "GET")
                 (url-request-extra-headers nil)
                 (url-cookie-storage nil)
                 (url-cookie-secure-storage nil)
                 (buf (url-retrieve-synchronously url t nil 10)))
            (when buf
              (unwind-protect
                  (with-current-buffer buf
                    (goto-char (point-min))
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (let ((data (buffer-substring-no-properties
                                   (point) (point-max))))
                        (when (> (length data) 0)
                          (with-temp-file cache-path
                            (set-buffer-multibyte nil)
                            (insert data))
                          cache-path))))
                (kill-buffer buf))))
        (error nil)))))

;;; Core lookup

(defun slacko-emoji--shortcode-to-unicode (shortcode)
  "Look up SHORTCODE (e.g. \":smile:\") and return its Unicode string, or nil."
  (when-let* ((entry (gethash shortcode emojify-emojis)))
    (gethash "unicode" entry)))

(defun slacko-emoji--resolve (shortcode host)
  "Resolve SHORTCODE to a display value for workspace HOST.
Returns one of:
  - A Unicode string (standard emoji)
  - An Emacs image descriptor (custom workspace emoji)
  - nil (unknown shortcode)"
  ;; 1. Try standard Unicode via emojify
  (or (slacko-emoji--shortcode-to-unicode shortcode)
      ;; 2. Try workspace custom emoji
      (when host
        (let* ((name (replace-regexp-in-string "^:\\|:$" "" shortcode))
               (url (slacko-emoji--resolve-alias name host)))
          (when url
            (when-let* ((path (slacko-emoji--download-emoji-image url host name)))
              (condition-case nil
                  (create-image path nil nil
                                :ascent 'center
                                :height (default-font-height))
                (error nil))))))))

;;; Overlay management

(defun slacko-emoji--make-overlay (beg end display shortcode)
  "Create an emoji overlay from BEG to END showing DISPLAY for SHORTCODE.
DISPLAY can be a string (Unicode) or an image descriptor."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'category 'slacko-emoji)
    (overlay-put ov 'display display)
    (overlay-put ov 'slacko-emoji-shortcode shortcode)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'help-echo shortcode)
    ov))

(defun slacko-emoji--make-count-overlay (beg end count-str)
  "Create a superscript overlay from BEG to END showing COUNT-STR.
BEG should include the leading space so it gets replaced by a thin space."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'category 'slacko-emoji)
    (overlay-put ov 'display
                 (propertize (concat "\u200B" count-str) ;; zero-width space + digits
                             'face 'slacko-emoji-count
                             'display '(raise 0.3)))
    (overlay-put ov 'slacko-emoji-count t)
    (overlay-put ov 'evaporate t)
    ov))

(defun slacko-emoji--emojify-region (beg end)
  "Scan region from BEG to END for :shortcode: patterns and place emoji overlays.
When a shortcode is followed by a space and a number (reaction count),
the count is rendered as superscript.
Uses `slacko-emoji--buffer-host' for resolving workspace custom emojis."
  (when (hash-table-p emojify-emojis)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\(:[a-z0-9_+-]+:\\)\\( \\([0-9]+\\)\\)?" end t)
        (let* ((shortcode (match-string 1))
               (mb (match-beginning 1))
               (me (match-end 1))
               (display (slacko-emoji--resolve shortcode slacko-emoji--buffer-host)))
          (when (and display
                     ;; don't double-overlay
                     (not (seq-some (lambda (ov)
                                      (eq (overlay-get ov 'category) 'slacko-emoji))
                                    (overlays-at mb))))
            (slacko-emoji--make-overlay mb me display shortcode)
            ;; superscript the count if present - overlay covers space+digits
            (when (match-beginning 2)
              (slacko-emoji--make-count-overlay
               (match-beginning 2) (match-end 2)
               (match-string 3)))))))))

(defun slacko-emoji--emojify-buffer ()
  "Place emoji overlays on the entire current buffer."
  (slacko-emoji--emojify-region (point-min) (point-max)))

(defun slacko-emoji--remove-overlays ()
  "Remove all slacko-emoji overlays from the current buffer."
  (setq slacko-emoji--revealed-overlays nil)
  (remove-overlays (point-min) (point-max) 'category 'slacko-emoji))

;;; Reveal-at-point (org-appear style)

(defun slacko-emoji--overlay-at-point ()
  "Return the slacko-emoji overlay at point, or nil."
  (seq-find (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
            (overlays-at (point))))

(defun slacko-emoji--reveal (ov)
  "Reveal the shortcode text for overlay OV by removing its display property.
Only reveals emoji overlays, not count overlays."
  (when (and ov
             (overlay-get ov 'display)
             (not (overlay-get ov 'slacko-emoji-count)))
    (overlay-put ov 'display nil)
    (push ov slacko-emoji--revealed-overlays)))

(defun slacko-emoji--conceal (ov)
  "Re-conceal overlay OV, restoring its emoji display."
  (when ov
    (when-let* ((shortcode (overlay-get ov 'slacko-emoji-shortcode))
                (display (slacko-emoji--resolve shortcode slacko-emoji--buffer-host)))
      (overlay-put ov 'display display))
    (setq slacko-emoji--revealed-overlays
          (delq ov slacko-emoji--revealed-overlays))))

(defun slacko-emoji--post-command ()
  "Reveal emoji at point; conceal previously revealed ones elsewhere."
  (let ((current-ov (slacko-emoji--overlay-at-point)))
    ;; conceal any previously revealed overlays that aren't at point
    (dolist (ov slacko-emoji--revealed-overlays)
      (unless (eq ov current-ov)
        (slacko-emoji--conceal ov)))
    ;; reveal the one at point
    (when current-ov
      (slacko-emoji--reveal current-ov))))

;;; After-change hook for dynamic content

(defun slacko-emoji--after-change (beg end _len)
  "Re-emojify the changed region from BEG to END."
  ;; expand to line boundaries for safety
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
        (line-end (save-excursion (goto-char end) (line-end-position))))
    ;; remove existing emoji overlays in the region first
    (dolist (ov (overlays-in line-beg line-end))
      (when (eq (overlay-get ov 'category) 'slacko-emoji)
        (delete-overlay ov)))
    (slacko-emoji--emojify-region line-beg line-end)))

;;; Interactive commands

(defun slacko-emoji-show-text-at-point ()
  "Show the original shortcode for the emoji at point."
  (interactive)
  (if-let* ((ov (slacko-emoji--overlay-at-point))
            (shortcode (overlay-get ov 'slacko-emoji-shortcode)))
      (message "%s" shortcode)
    (message "No emoji at point")))

;;; Minor mode

;;;###autoload
(define-minor-mode slacko-emoji-mode
  "Toggle emoji overlay display in the current buffer.
When enabled, Slack shortcodes like :smile: are overlaid with their
Unicode emoji equivalents.  Custom workspace emojis are rendered as
inline images.  The original text is revealed when the cursor is on
an emoji (org-appear style)."
  :lighter " E"
  :group 'slacko-emoji
  (if slacko-emoji-mode
      (progn
        ;; ensure emojify data is loaded
        (unless (and (boundp 'emojify-emojis)
                     (hash-table-p emojify-emojis)
                     (> (hash-table-count emojify-emojis) 0))
          (emojify-create-emojify-emojis))
        ;; fetch workspace custom emojis if we know the host
        (when slacko-emoji--buffer-host
          (slacko-emoji--fetch-workspace-emojis slacko-emoji--buffer-host))
        (slacko-emoji--emojify-buffer)
        (add-hook 'post-command-hook #'slacko-emoji--post-command nil t)
        (add-hook 'after-change-functions #'slacko-emoji--after-change nil t))
    (slacko-emoji--remove-overlays)
    (remove-hook 'post-command-hook #'slacko-emoji--post-command t)
    (remove-hook 'after-change-functions #'slacko-emoji--after-change t)))

;;; Auto-enable in GUI

(defun slacko-emoji--maybe-enable ()
  "Enable `slacko-emoji-mode' if running in a graphical display."
  (when (display-graphic-p)
    (slacko-emoji-mode 1)))

(with-eval-after-load 'slacko
  (add-hook 'slacko-search-mode-hook #'slacko-emoji--maybe-enable))

(with-eval-after-load 'slacko-thread
  (add-hook 'slacko-thread-mode-hook #'slacko-emoji--maybe-enable))

(provide 'slacko-emoji)
;;; slacko-emoji.el ends here
