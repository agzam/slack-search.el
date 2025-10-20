;;; slack-search.el --- Search in Slack -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 18, 2025
;; Modified: October 18, 2025
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
;;  Search in Slack
;;  
;;
;;; Code:

(require 'url)
(require 'json)
(require 'org)
(require 'slack-mrkdwn)

;;; Customizable Variables

(defgroup slack-search nil
  "Search Slack messages."
  :group 'tools
  :prefix "slack-search-")

(defcustom slack-search-token-function nil
  "Function that returns the Slack API token.
Should return a string containing the Bearer token."
  :type 'function
  :group 'slack-search)

(defcustom slack-search-cookie-function nil
  "Function that returns the Slack cookie.
Should return a string containing the `d' cookie value."
  :type 'function
  :group 'slack-search)

(defcustom slack-search-buffer-name "*Slack Search*"
  "Name of the buffer to display search results."
  :type 'string
  :group 'slack-search)

(defcustom slack-search-results-per-page 20
  "Number of results to fetch per page."
  :type 'integer
  :group 'slack-search)

(defcustom slack-search-inhibit-redirect-browser-tab t
  "Whether to close the redirect browser tab after opening Slack link.
When non-nil, automatically close the browser tab that Slack opens
for redirection after the link opens in the Slack app.  This prevents
lingering useless tabs in your browser.  Only works on macOS."
  :type 'boolean
  :group 'slack-search)

(defcustom slack-search-browser-name "Brave Browser"
  "Name of the browser application to close tabs in.
Common values: \"Brave Browser\", \"Google Chrome\", \"Safari\".
Only used when `slack-search-inhibit-redirect-browser-tab' is non-nil."
  :type 'string
  :group 'slack-search)

(defcustom slack-search-close-tab-delay 0.5
  "Delay in seconds before closing the browser tab after opening Slack link.
Only used when `slack-search-inhibit-redirect-browser-tab' is non-nil."
  :type 'number
  :group 'slack-search)

;;; Internal Variables

(defvar slack-search--current-query nil
  "The current search query.")

(defvar slack-search--current-page 1
  "Current page number for pagination.")

(defvar slack-search--total-pages nil
  "Total number of pages available.")

(defvar slack-search--loading nil
  "Flag indicating if a search is currently loading.")

;;; Org-mode Link Handler

(defun slack-search--follow-link (path)
  "Open Slack link from PATH and close browser tab.
PATH should be the part after slack:// prefix."
  (let ((url (concat "https:" path)))
    ;; Open the URL (which will redirect to Slack app)
    (browse-url url)
    ;; Close the browser tab after a delay (macOS only)
    (when (and slack-search-inhibit-redirect-browser-tab
               (eq system-type 'darwin))
      (run-at-time slack-search-close-tab-delay nil
                   (lambda ()
                     (let ((jxa-script (format "Application('%s').windows[0].activeTab.close(); Application('Slack').activate();"
                                               slack-search-browser-name)))
                       (shell-command (format "osascript -l JavaScript -e \"%s\"" jxa-script))))))))

;; Register the slack:// link type with org-mode
(org-link-set-parameters
 "slack"
 :follow #'slack-search--follow-link)

;;; Helper Functions



(defun slack-search--get-token ()
  "Get the Slack API token."
  (if slack-search-token-function
      (funcall slack-search-token-function)
    (error "`slack-search-token-function` is not set")))

(defun slack-search--get-cookie ()
  "Get the Slack cookie."
  (if slack-search-cookie-function
      (funcall slack-search-cookie-function)
    (error "`slack-search-cookie-function` is not set")))

(defun slack-search--make-request (query page callback)
  "Send a search request to Slack API for QUERY at PAGE.
Call CALLBACK with the parsed JSON response."
  (let* ((url-request-method "GET")
         (token (slack-search--get-token))
         (cookie (slack-search--get-cookie))
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" token))
            ("Cookie" . ,(format "d=%s; " cookie))
            ("Content-Type" . "application/json")))
         (query-params
          (url-build-query-string
           `((query ,query)
             (count ,(number-to-string slack-search-results-per-page))
             (page ,(number-to-string page)))))
         (url (format "https://slack.com/api/search.messages?%s" query-params))
         ;; Prevent url-retrieve from adding cookies automatically
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil))
    ;; (message "Request URL: %s" url)
    ;; (message "Headers: %S" url-request-extra-headers)
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "Slack search error: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let* ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (response (json-read)))
           ;; (message "Response: %S" response)
           (funcall callback response))))
     nil t)))

(defun slack-search--parse-result (match)
  "Parse a single search result MATCH into display format."
  (let* ((username (alist-get 'username match))
         (user-id (alist-get 'user match))
         (text (alist-get 'text match))
         (attachments (alist-get 'attachments match))
         (channel (alist-get 'channel match))
         (channel-id (when channel (alist-get 'id channel)))
         (channel-name (if channel
                           (alist-get 'name channel)
                         "unknown"))
         ;; Determine conversation type
         (is-channel (when channel (eq (alist-get 'is_channel channel) t)))
         (is-group (when channel (eq (alist-get 'is_group channel) t)))
         (is-im (when channel (eq (alist-get 'is_im channel) t)))
         (is-mpim (when channel (eq (alist-get 'is_mpim channel) t)))
         (conversation-type (cond
                             (is-im "DM")
                             (is-mpim "Group DM")
                             (is-group "Private Channel")
                             (is-channel "Channel")
                             (t "Unknown")))
         (ts (alist-get 'ts match))
         (timestamp (when (and ts (stringp ts))
                      (condition-case nil
                          (format-time-string "%b %d at %I:%M %p"
                                              (seconds-to-time (string-to-number ts)))
                        (error "Unknown date"))))
         ;; Use the permalink from the API response directly
         (permalink (alist-get 'permalink match))
         ;; Extract workspace URL from permalink
         (workspace-url (when (stringp permalink)
                          (and (string-match "\\(https://[^/]+\\)" permalink)
                               (match-string 1 permalink))))
         (thread-ts (alist-get 'thread_ts match))
         (thread-info (if thread-ts "Thread" ""))
         ;; Handle attachments (e.g., shared messages from other channels)
         (attachment-info (when (and attachments (listp attachments))
                            (let ((first-attach (car attachments)))
                              (when first-attach
                                (list :is-share (alist-get 'is_share first-attach)
                                      :author-name (alist-get 'author_name first-attach)
                                      :author-id (alist-get 'author_id first-attach)
                                      :channel-id (alist-get 'channel_id first-attach)
                                      :from-url (alist-get 'from_url first-attach)
                                      :attach-text (or (alist-get 'text first-attach)
                                                      (alist-get 'fallback first-attach)))))))
         ;; Get text content: prefer main text, fall back to attachment text
         (content-text (if (and (stringp text) (not (string-empty-p text)))
                           text
                         (when attachment-info
                           (plist-get attachment-info :attach-text))))
         ;; Handle files
         (files (alist-get 'files match))
         (files-info (when (and files (listp files))
                       (mapcar (lambda (file)
                                 (list :name (alist-get 'name file)
                                       :permalink (alist-get 'permalink file)
                                       :size (alist-get 'size file)
                                       :pretty-type (alist-get 'pretty_type file)))
                               files))))
    (list :author (if (stringp username) username "Unknown")
          :user-id (if (stringp user-id) user-id nil)
          :workspace-url (if (stringp workspace-url) workspace-url nil)
          :channel (if (stringp channel-name) channel-name "unknown")
          :channel-id (if (stringp channel-id) channel-id nil)
          :conversation-type conversation-type
          :timestamp (if (stringp timestamp) timestamp "unknown date")
          :permalink (if (stringp permalink) permalink "")
          :thread thread-info
          :text (if (stringp content-text) (slack-mrkdwn-to-org content-text) "")
          :attachment-info attachment-info
          :files files-info)))

(defun slack-search--insert-result (result)
  "Insert a single RESULT into the current buffer."
  (let* ((author (plist-get result :author))
         (user-id (plist-get result :user-id))
         (workspace-url (plist-get result :workspace-url))
         (channel (plist-get result :channel))
         (channel-id (plist-get result :channel-id))
         (conversation-type (plist-get result :conversation-type))
         (timestamp (plist-get result :timestamp))
         (permalink (plist-get result :permalink))
         (thread (plist-get result :thread))
         (text (plist-get result :text))
         (attachment-info (plist-get result :attachment-info))
         (files (plist-get result :files))
         ;; Ensure all values are strings
         (author-str (if (stringp author) author "Unknown"))
         (channel-str (if (stringp channel) channel "unknown"))
         (timestamp-str (if (stringp timestamp) timestamp "unknown date"))
         ;; Convert https:// to slack:// for custom link handler
         (permalink-str (if (stringp permalink)
                            (replace-regexp-in-string "^https:" "slack:" permalink)
                          "slack:#"))
         (text-str (if (stringp text) text ""))
         ;; Construct user profile link
         (author-link (if (and (stringp user-id) (stringp workspace-url))
                          (format "[[slack://%s/team/%s][%s]]"
                                  (replace-regexp-in-string "^https://" "" workspace-url)
                                  user-id
                                  author-str)
                        author-str))
         ;; Construct channel link - use conversation type for non-channels
         (channel-link (if (and (stringp channel-id) (stringp workspace-url))
                           (let ((link-text (if (string= conversation-type "Channel")
                                                (format "#%s" channel-str)
                                              conversation-type)))
                             (format "[[slack://%s/archives/%s][%s]]"
                                     (replace-regexp-in-string "^https://" "" workspace-url)
                                     channel-id
                                     link-text))
                         (format "#%s" channel-str)))
         ;; Handle shared message info
         (is-shared (and attachment-info (plist-get attachment-info :is-share)))
         (share-metadata (when is-shared
                           (let* ((orig-author (plist-get attachment-info :author-name))
                                  (orig-author-id (plist-get attachment-info :author-id))
                                  (orig-channel-id (plist-get attachment-info :channel-id))
                                  (orig-url (plist-get attachment-info :from-url)))
                             ;; Extract timestamp from the from-url
                             ;; URL format: https://workspace.slack.com/archives/CHANNEL_ID/pTIMESTAMP
                             (when (and orig-url (string-match "/archives/\\([^/]+\\)/p\\([0-9]+\\)" orig-url))
                               (let* ((orig-ts (match-string 2 orig-url))
                                      (orig-timestamp (when orig-ts
                                                        (condition-case nil
                                                            (format-time-string "%b %d at %I:%M %p"
                                                                                (seconds-to-time (string-to-number (substring orig-ts 0 10))))
                                                          (error nil))))
                                      ;; Build author link
                                      (orig-author-link (if (and orig-author-id workspace-url)
                                                            (format "[[slack://%s/team/%s][%s]]"
                                                                    (replace-regexp-in-string "^https://" "" workspace-url)
                                                                    orig-author-id
                                                                    (or orig-author "Unknown"))
                                                          (or orig-author "Unknown")))
                                      ;; Build channel link
                                      (orig-channel-link (if (and orig-channel-id workspace-url)
                                                             (format "[[slack://%s/archives/%s][#%s]]"
                                                                     (replace-regexp-in-string "^https://" "" workspace-url)
                                                                     orig-channel-id
                                                                     orig-channel-id)
                                                           "#unknown")))
                                 (format "** /Shared from %s | Posted in %s | [[%s][%s]]/\n"
                                         orig-author-link
                                         orig-channel-link
                                         (replace-regexp-in-string "^https:" "slack:" orig-url)
                                         (or orig-timestamp "unknown date")))))))
         ;; Format files list
         (files-str (when files
                      (concat "\n"
                              (mapconcat
                               (lambda (file)
                                 (let* ((name (plist-get file :name))
                                        (permalink (plist-get file :permalink))
                                        (size (plist-get file :size))
                                        (type (plist-get file :pretty-type))
                                        ;; Convert bytes to human-readable format
                                        (size-str (cond
                                                   ((not size) "")
                                                   ((< size 1024) (format "%d B" size))
                                                   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
                                                   (t (format "%.1f MB" (/ size 1024.0 1024.0))))))
                                   (format "- [[%s][%s]] (%s%s)"
                                           permalink
                                           name
                                           size-str
                                           (if type (format ", %s" type) ""))))
                               files
                               "\n")
                              "\n"))))
    (if is-shared
        ;; For shared messages, use a subheading with the shared metadata
        (insert (format "* %s | %s | [[%s][%s]]\n\n%s\n%s%s\n"
                        author-link
                        channel-link
                        permalink-str
                        timestamp-str
                        share-metadata
                        (if (not (string-empty-p text-str))
                            text-str
                          "")
                        (or files-str "")))
      ;; For regular messages, use standard format
      (insert (format "* %s | %s | [[%s][%s]]\n\n%s%s\n"
                      author-link
                      channel-link
                      permalink-str
                      timestamp-str
                      text-str
                      (or files-str ""))))))

(defun slack-search--display-results (response &optional append)
  "Display search results from RESPONSE in `org-mode' buffer.
If APPEND is non-nil, append to existing results."
  (let* ((ok (alist-get 'ok response))
         (messages-data (alist-get 'messages response))
         (matches (alist-get 'matches messages-data))
         (total (alist-get 'total messages-data))
         (paging (alist-get 'paging messages-data))
         (page (alist-get 'page paging))
         (pages (alist-get 'pages paging)))
    ;; (message "DEBUG: ok=%s, matches count=%s, total=%s" ok (length matches) total)
    (if (not ok)
        (message "Slack search failed: %s" (alist-get 'error response))
      
      (let ((buffer (get-buffer-create slack-search-buffer-name)))
        (with-current-buffer buffer
        (let ((saved-point (when append (point)))
              (saved-window-start (when append
                                    (and (get-buffer-window (current-buffer))
                                         (window-start (get-buffer-window (current-buffer)))))))
          
          (unless append
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "#+TITLE: Slack Search Results for: %s\n" slack-search--current-query))
              (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert (format "Total results: %d\n\n" (or total 0)))))
          
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (dolist (match matches)
              (slack-search--insert-result (slack-search--parse-result match)))
            
            ;; Indent the entire buffer properly
            (indent-region (point-min) (point-max)))
          
          (setq slack-search--current-page page
                slack-search--total-pages pages)
          
          ;; Restore position when appending, otherwise set up the buffer
          (if append
              (progn
                (when saved-point (goto-char saved-point))
                (when saved-window-start
                  (set-window-start (get-buffer-window (current-buffer)) saved-window-start)))
            (unless (eq major-mode 'slack-search-mode)
              (slack-search-mode))
            (goto-char (point-min)))
          
          ;; Load next page if available, but only after appending results
          ;; For the first page, don't auto-load to avoid race conditions
          (when (and append (< page pages))
            (slack-search--load-next-page))))
        
        ;; Switch to buffer AFTER all processing is complete
        (unless append
          (switch-to-buffer buffer)
          (set-window-start (selected-window) (point-min)))))))

(defun slack-search--load-next-page ()
  "Load the next page of search results asynchronously."
  (unless slack-search--loading
    (setq slack-search--loading t)
    (let ((next-page (1+ slack-search--current-page)))
      ;; (message "Loading page %d of %d..." next-page slack-search--total-pages)
      (slack-search--make-request
       slack-search--current-query
       next-page
       (lambda (response)
         (setq slack-search--loading nil)
         (slack-search--display-results response t)
         ;; (message "Loaded page %d of %d" slack-search--current-page slack-search--total-pages)
         )))))

;;; Major Mode

(defvar slack-search-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map
    (set-keymap-parent map org-mode-map)
    ;; Add custom keybindings here if needed
    ;; (define-key map (kbd "C-c C-n") #'slack-search-next-result)
    ;; (define-key map (kbd "C-c C-p") #'slack-search-previous-result)
    map)
  "Keymap for `slack-search-mode'.")

(define-derived-mode slack-search-mode org-mode "Slack-Search"
  "Major mode for displaying Slack search results.

This mode is derived from `org-mode' and displays search results
from Slack in an organized, readable format. Each result includes
author, channel, timestamp, and message content with proper formatting.

\\{slack-search-mode-map}"
  ;; Set buffer to read-only by default
  (setq buffer-read-only t)
  ;; Add any additional mode-specific setup here
  )

;;; Interactive Commands

;;;###autoload
(defun slack-search (query)
  "Search Slack messages for QUERY.
Display results in an `org-mode' buffer with pagination."
  (interactive "sSearch Slack: ")
  (setq slack-search--current-query query
        slack-search--current-page 1
        slack-search--total-pages nil
        slack-search--loading nil)
  ;; (message "Searching Slack for: %s" query)
  (slack-search--make-request
   query
   1
   (lambda (response)
     (slack-search--display-results response nil))))

(provide 'slack-search)
;;; slack-search.el ends here
