;;; slacko-render.el --- Unified message rendering for Slacko -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2026
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/agzam/slacko
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Shared message rendering for all Slacko views (search, thread, etc.).
;; Provides a single `slacko-render-message' entry point that both
;; `slacko-search-mode' and `slacko-thread-mode' use.  Also houses
;; user/channel mention resolution, inline image display, and
;; timestamp formatting.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'slacko-mrkdwn)
(require 'slacko-creds)

;;; Customizable Variables

(defcustom slacko-render-inline-images t
  "Whether to display images inline in message buffers.
When non-nil, image files are downloaded and shown inline.
When nil, images are shown as regular file links.
Only works when workspace credentials are available."
  :type 'boolean
  :group 'slacko)

(defcustom slacko-render-image-max-width 480
  "Maximum width in pixels for inline images.
Images wider than this will be scaled down."
  :type 'integer
  :group 'slacko)

(defcustom slacko-render-timestamp-format "%Y-%m-%d %H:%M"
  "Format string for message timestamps.
See `format-time-string' for available format specifiers."
  :type 'string
  :group 'slacko)

;;; User Resolution

(defvar slacko-render--user-cache (make-hash-table :test 'equal)
  "Cache of user-id -> display-name, keyed as \"host:user-id\".")

(defun slacko-render-resolve-user (host user-id)
  "Resolve USER-ID to a display name for workspace HOST.
Results are cached.  Returns USER-ID if resolution fails."
  (let ((cache-key (format "%s:%s" host user-id)))
    (or (gethash cache-key slacko-render--user-cache)
        (condition-case nil
            (let* ((resp (slacko-creds-api-request
                          host "users.info"
                          `((user ,user-id))))
                   (user (alist-get 'user resp))
                   (profile (alist-get 'profile user))
                   (name (or (alist-get 'display_name profile)
                             (alist-get 'real_name user)
                             (alist-get 'name user)
                             user-id)))
              (puthash cache-key name slacko-render--user-cache)
              name)
          (error user-id)))))

(defun slacko-render-resolve-user-mentions (host text)
  "Replace <@USER_ID> mentions in TEXT with display names for HOST.
If HOST is nil or credentials unavailable, return TEXT unchanged."
  (if (and host text (string-match-p "<@U[A-Z0-9]+>" text))
      (condition-case nil
          (let ((result text))
            (while (string-match "<@\\(U[A-Z0-9]+\\)>" result)
              (let* ((uid (match-string 1 result))
                     (name (save-match-data
                             (slacko-render-resolve-user host uid)))
                     (replacement (format "@%s" name)))
                (setq result (replace-match replacement t t result))))
            result)
        (error text))
    text))

;;; Channel Resolution

(defvar slacko-render--channel-cache (make-hash-table :test 'equal)
  "Cache of channel-id -> channel-name, keyed as \"host:channel-id\".")

(defun slacko-render-resolve-channel (host channel-id)
  "Resolve CHANNEL-ID to a channel name for workspace HOST.
Results are cached.  Returns CHANNEL-ID if resolution fails."
  (let ((cache-key (format "%s:%s" host channel-id)))
    (or (gethash cache-key slacko-render--channel-cache)
        (condition-case nil
            (let* ((resp (slacko-creds-api-request
                          host "conversations.info"
                          `((channel ,channel-id))))
                   (channel (alist-get 'channel resp))
                   (name (or (alist-get 'name channel) channel-id)))
              (puthash cache-key name slacko-render--channel-cache)
              name)
          (error channel-id)))))

(defun slacko-render-resolve-channel-mentions (host text)
  "Replace <#CHANNEL_ID|name> mentions in TEXT with org links.
When name is provided after the pipe, use it directly.
Otherwise, resolve via API (cached).
If HOST is nil or credentials unavailable, return TEXT unchanged."
  (if (and host text (string-match-p "<#C[A-Z0-9]+|" text))
      (condition-case nil
          (let ((result text))
            (while (string-match "<#\\(C[A-Z0-9]+\\)|\\([^>]*\\)>" result)
              (let* ((cid (match-string 1 result))
                     (inline-name (match-string 2 result))
                     (name (if (and inline-name
                                    (not (string-empty-p inline-name)))
                               inline-name
                             (save-match-data
                               (slacko-render-resolve-channel host cid))))
                     (replacement (format "[[slack://%s/archives/%s][#%s]]"
                                          host cid name)))
                (setq result (replace-match replacement t t result))))
            result)
        (error text))
    text))

;;; Images

(defun slacko-render--image-url-for-file (file)
  "Return the best thumbnail URL for FILE, or nil if not an image.
Prefers thumb_480, falls back to thumb_360, then url_private."
  (let ((mimetype (alist-get 'mimetype file)))
    (when (and mimetype (string-prefix-p "image/" mimetype))
      (or (alist-get 'thumb_480 file)
          (alist-get 'thumb_360 file)
          (alist-get 'thumb_720 file)
          (alist-get 'url_private file)))))

(defun slacko-render--download-image (url host)
  "Download image at URL using credentials for HOST.
Returns image data as a string, or nil on failure."
  (condition-case nil
      (let* ((token (slacko-creds-get host "token"))
             (cookie (slacko-creds-get host "cookie"))
             (url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(format "Bearer %s" token))
                ("Cookie" . ,(format "d=%s;" cookie))))
             (url-cookie-storage nil)
             (url-cookie-secure-storage nil)
             (buf (url-retrieve-synchronously url t nil 15)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer buf))))
    (error nil)))

(defun slacko-render--insert-image (file host)
  "Insert an inline image for FILE using credentials for HOST.
Returns non-nil if the image was successfully inserted."
  (when-let* ((img-url (slacko-render--image-url-for-file file))
              (data (slacko-render--download-image img-url host))
              (img (create-image data nil t
                                 :max-width slacko-render-image-max-width
                                 :scale 1.0)))
    (insert-image img (format "[%s]" (or (alist-get 'name file) "image")))
    (insert "\n")
    t))

;;; Timestamp

(defun slacko-render-format-timestamp (ts)
  "Format a Slack timestamp TS into a human-readable string."
  (when (and ts (stringp ts))
    (ignore-errors
      (format-time-string slacko-render-timestamp-format
                          (seconds-to-time (string-to-number ts))))))

;;; Rendering Helpers

(defun slacko-render--build-author-link (host author author-id)
  "Build an org link for AUTHOR with AUTHOR-ID on HOST.
Returns a plain AUTHOR string if linking is not possible."
  (if (and host author-id)
      (format "[[slack://%s/team/%s][%s]]" host author-id author)
    author))

(defun slacko-render--build-channel-link (host channel-name channel-id
                                               conversation-type)
  "Build an org link for a channel.
Uses CHANNEL-NAME, CHANNEL-ID, HOST, and CONVERSATION-TYPE.
Returns nil when channel info is not available."
  (when (and host channel-id)
    (let ((link-text (if (and conversation-type
                              (not (string= conversation-type "Channel")))
                         conversation-type
                       (format "#%s" (or channel-name "unknown")))))
      (format "[[slack://%s/archives/%s][%s]]" host channel-id link-text))))

(defun slacko-render--format-file-size (size)
  "Format file SIZE in bytes to human-readable string."
  (cond
   ((not size) "")
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%.1f MB" (/ size 1024.0 1024.0)))))

(defun slacko-render--insert-files (files host)
  "Insert file attachments for FILES using HOST for image auth."
  (dolist (file files)
    (let* ((name (or (alist-get 'name file) "file"))
           (url (or (alist-get 'url_private file)
                    (alist-get 'permalink file)
                    ""))
           (ptype (alist-get 'pretty_type file))
           (mimetype (alist-get 'mimetype file))
           (size (alist-get 'size file)))
      ;; Try inline image first for image files
      (if (and slacko-render-inline-images
               host mimetype
               (string-prefix-p "image/" mimetype)
               (slacko-render--insert-image file host))
          ;; Link below the image for reference
          (insert (format "  /[[%s][%s]]/\n" url name))
        ;; Fallback: regular link with size info
        (let ((size-str (slacko-render--format-file-size size)))
          (insert (format "- [[%s][%s]]%s\n"
                          url name
                          (cond
                           ((and (not (string-empty-p size-str)) ptype)
                            (format " (%s, %s)" size-str ptype))
                           ((not (string-empty-p size-str))
                            (format " (%s)" size-str))
                           (ptype (format " (%s)" ptype))
                           (t "")))))))))

(defun slacko-render--insert-reactions (reactions)
  "Insert REACTIONS as a formatted line."
  (when reactions
    (insert (mapconcat
             (lambda (r)
               (format ":%s: %d" (alist-get 'name r) (alist-get 'count r)))
             reactions "  ")
            "\n")))

(defun slacko-render--insert-share-info (share-info host)
  "Insert shared message sub-heading from SHARE-INFO plist.
HOST is used for building links."
  (when share-info
    (let* ((orig-author (plist-get share-info :author-name))
           (orig-author-id (plist-get share-info :author-id))
           (orig-channel-id (plist-get share-info :channel-id))
           (orig-url (plist-get share-info :from-url)))
      (when (and orig-url
                 (string-match "/archives/\\([^/]+\\)/p\\([0-9]+\\)" orig-url))
        (let* ((orig-ts (match-string 2 orig-url))
               (orig-timestamp
                (when orig-ts
                  (ignore-errors
                    (slacko-render-format-timestamp
                     (concat (substring orig-ts 0 -6) "."
                             (substring orig-ts -6))))))
               (orig-author-link
                (slacko-render--build-author-link
                 host (or orig-author "Unknown") orig-author-id))
               (orig-channel-link
                (if (and host orig-channel-id)
                    (format "[[slack://%s/archives/%s][#%s]]"
                            host orig-channel-id orig-channel-id)
                  "#unknown"))
               (orig-permalink
                (replace-regexp-in-string "^https:" "slack:" orig-url)))
          (insert (format "** /Shared from %s | Posted in %s | [[%s][%s]]/\n"
                          orig-author-link
                          orig-channel-link
                          orig-permalink
                          (or orig-timestamp "unknown date"))))))))

;;; Main Rendering Entry Point

(defun slacko-render-message (msg)
  "Render a normalized message MSG into the current buffer.

MSG is a plist with these keys:

  :author       - display name string
  :author-id    - user ID (for building profile link)
  :text         - raw mrkdwn text (will be converted to org)
  :ts           - raw Slack timestamp string
  :permalink    - slack:// URL string
  :level        - org heading level (integer, default 1)
  :files        - list of file alists from API (optional)
  :reactions    - list of reaction alists from API (optional)
  :host         - workspace host for API calls (may be nil)
  :channel-name - channel name (optional)
  :channel-id   - channel ID (optional)
  :conversation-type - \"Channel\", \"DM\", etc. (optional)
  :share-info   - shared message metadata plist (optional)"
  (let* ((author (or (plist-get msg :author) "Unknown"))
         (author-id (plist-get msg :author-id))
         (raw-text (plist-get msg :text))
         (ts (plist-get msg :ts))
         (permalink (or (plist-get msg :permalink) ""))
         (level (or (plist-get msg :level) 1))
         (files (plist-get msg :files))
         (reactions (plist-get msg :reactions))
         (host (plist-get msg :host))
         (channel-name (plist-get msg :channel-name))
         (channel-id (plist-get msg :channel-id))
         (conv-type (plist-get msg :conversation-type))
         (share-info (plist-get msg :share-info))
         ;; Process text through the full pipeline
         (text (slacko-render-resolve-user-mentions host raw-text))
         (text (slacko-render-resolve-channel-mentions host text))
         (text (if text (slacko-mrkdwn-to-org text) ""))
         ;; Build display elements
         (stars (make-string level ?*))
         (timestamp (or (slacko-render-format-timestamp ts) "unknown date"))
         (author-link (slacko-render--build-author-link
                       host author author-id))
         (channel-link (slacko-render--build-channel-link
                        host channel-name channel-id conv-type)))
    ;; Heading
    (if channel-link
        (insert (format "%s %s | %s | [[%s][%s]]\n"
                        stars author-link channel-link permalink timestamp))
      (insert (format "%s %s | [[%s][%s]]\n"
                      stars author-link permalink timestamp)))
    ;; Share info sub-heading
    (slacko-render--insert-share-info share-info host)
    ;; Text body
    (unless (string-empty-p text)
      (insert text "\n"))
    ;; Files
    (when files
      (slacko-render--insert-files files host))
    ;; Reactions
    (slacko-render--insert-reactions reactions)
    ;; Trailing newline
    (insert "\n")))

(defun slacko-render-setup-font-lock ()
  "Set up font-lock keywords for a Slacko buffer.
Call this from mode definitions."
  (font-lock-add-keywords nil slacko-mrkdwn-font-lock-keywords))

(provide 'slacko-render)
;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-render.el ends here
