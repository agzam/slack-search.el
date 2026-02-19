;;; slacko.el --- Search in Slack -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 18, 2025
;; Modified: January 20, 2025
;; Version: 1.0.0
;; Keywords: tools
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  El Slacko - a Slack reader for Emacs
;;  
;;
;;; Code:

(require 'json)
(require 'org)
(require 'slacko-mrkdwn)
(require 'slacko-render)
(require 'slacko-creds)

;;; Customizable Variables

(defgroup slacko nil
  "Search Slack messages."
  :group 'tools
  :prefix "slacko-")

(defcustom slacko-default-host nil
  "Default Slack workspace host for search (e.g. \"myteam.slack.com\").
When nil, uses the first workspace found in the credentials file."
  :type '(choice string (const nil))
  :group 'slacko)

(defcustom slacko-search-buffer-name "*Slack Search*"
  "Name of the buffer to display search results."
  :type 'string
  :group 'slacko)

(defcustom slacko-search-results-per-page 20
  "Number of results to fetch per page."
  :type 'integer
  :group 'slacko)

(defcustom slacko-inhibit-redirect-browser-tab t
  "Whether to close the redirect browser tab after opening Slack link.
When non-nil, automatically close the browser tab that Slack opens
for redirection after the link opens in the Slack app.  This prevents
lingering useless tabs in your browser.  Only works on macOS."
  :type 'boolean
  :group 'slacko)

(defcustom slacko-browser-name "Brave Browser"
  "Name of the browser application to close tabs in.
Common values: \"Brave Browser\", \"Google Chrome\", \"Safari\".
Only used when `slacko-inhibit-redirect-browser-tab' is non-nil."
  :type 'string
  :group 'slacko)

(defcustom slacko-close-tab-delay 0.5
  "Delay in seconds before closing the browser tab after opening Slack link.
Only used when `slacko-inhibit-redirect-browser-tab' is non-nil."
  :type 'number
  :group 'slacko)

;;; Internal Variables

(defvar slacko--current-query nil
  "The current search query.")

(defvar slacko--current-page 1
  "Current page number for pagination.")

(defvar slacko--total-pages nil
  "Total number of pages available.")

(defvar slacko--loading nil
  "Flag indicating if a search is currently loading.")

;;; Org-mode Link Handler

(defun slacko--follow-link (path)
  "Open Slack link from PATH and close browser tab.
PATH should be the part after slack:// prefix."
  (let ((url (concat "https:" path)))
    ;; Open the URL (which will redirect to Slack app)
    (browse-url url)
    ;; Close the browser tab after a delay (macOS only)
    (when (and slacko-inhibit-redirect-browser-tab
               (eq system-type 'darwin))
      (run-at-time slacko-close-tab-delay nil
                   (lambda ()
                     (let ((jxa-script (format "Application('%s').windows[0].activeTab.close(); Application('Slack').activate();"
                                               slacko-browser-name)))
                       (shell-command (format "osascript -l JavaScript -e \"%s\"" jxa-script))))))))

;; Register the slack:// link type with org-mode
(org-link-set-parameters
 "slack"
 :follow #'slacko--follow-link)

;;; Helper Functions

(defun slacko--default-host ()
  "Return the default workspace host for search.
Uses `slacko-default-host' if set, otherwise discovers the first
workspace from the credentials file."
  (or slacko-default-host
      (let* ((auth-sources (append (list slacko-creds-gpg-file)
                                   (when (and (not (string= slacko-creds-gpg-file
                                                            slacko-creds--legacy-gpg-file))
                                              (file-exists-p slacko-creds--legacy-gpg-file))
                                     (list slacko-creds--legacy-gpg-file))))
             (auth-source-cache-expiry 0)
             (found (car (auth-source-search :user "token" :max 1))))
        (when found
          (plist-get found :host)))
      (error "No Slack workspace found. Run `slacko-creds-refresh' or set `slacko-default-host'")))

(defun slacko--make-request (query page)
  "Send a search request to Slack API for QUERY at PAGE.
Returns parsed JSON response or nil."
  (let ((host (slacko--default-host)))
    (slacko-creds-api-request
     host "search.messages"
     `((query ,query)
       (count ,(number-to-string slacko-search-results-per-page))
       (page ,(number-to-string page))))))

(defun slacko--parse-result (match)
  "Normalize a search result MATCH into a plist for `slacko-render-message'."
  (let* ((username (alist-get 'username match))
         (user-id (alist-get 'user match))
         (text (alist-get 'text match))
         (attachments (alist-get 'attachments match))
         (channel (alist-get 'channel match))
         (channel-id (when channel (alist-get 'id channel)))
         (channel-name (when channel (alist-get 'name channel)))
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
         (permalink (alist-get 'permalink match))
         ;; Derive host from permalink
         (host (when (and (stringp permalink)
                          (string-match "https://\\([^/]+\\)" permalink))
                 (match-string 1 permalink)))
         ;; Convert permalink to slack://
         (slack-permalink (when (stringp permalink)
                           (replace-regexp-in-string "^https:" "slack:" permalink)))
         ;; Handle attachments (shared messages)
         (share-info (when (and attachments (listp attachments))
                       (let ((first-attach (car attachments)))
                         (when (and first-attach
                                    (alist-get 'is_share first-attach))
                           (list :author-name (alist-get 'author_name first-attach)
                                 :author-id (alist-get 'author_id first-attach)
                                 :channel-id (alist-get 'channel_id first-attach)
                                 :from-url (alist-get 'from_url first-attach))))))
         ;; Get text: prefer main text, fall back to attachment text
         (content-text (if (and (stringp text) (not (string-empty-p text)))
                           text
                         (when (and attachments (listp attachments))
                           (let ((first-attach (car attachments)))
                             (when first-attach
                               (or (alist-get 'text first-attach)
                                   (alist-get 'fallback first-attach)))))))
         ;; Files (pass raw alists - renderer handles them)
         (files (alist-get 'files match))
         ;; Reactions
         (reactions (alist-get 'reactions match)))
    (list :author (if (stringp username) username "Unknown")
          :author-id (when (stringp user-id) user-id)
          :text (or content-text "")
          :ts ts
          :permalink (or slack-permalink "")
          :level 1
          :files files
          :reactions reactions
          :host host
          :channel-name (when (stringp channel-name) channel-name)
          :channel-id (when (stringp channel-id) channel-id)
          :conversation-type conversation-type
          :share-info share-info)))



(defun slacko--display-results (response &optional append)
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
      
      (let ((buffer (get-buffer-create slacko-search-buffer-name)))
        (with-current-buffer buffer
        (let ((saved-point (when append (point)))
              (saved-window-start (when append
                                    (and (get-buffer-window (current-buffer))
                                         (window-start (get-buffer-window (current-buffer)))))))
          
          (unless append
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "#+TITLE: Slack Search Results for: %s\n" slacko--current-query))
              (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert (format "Total results: %d\n\n" (or total 0)))))
          
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (dolist (match matches)
              (slacko-render-message (slacko--parse-result match)))
            
            ;; Indent the entire buffer properly
            (indent-region (point-min) (point-max)))
          
          (setq slacko--current-page page
                slacko--total-pages pages)
          
          ;; Restore position when appending, otherwise set up the buffer
          (if append
              (progn
                (when saved-point (goto-char saved-point))
                (when saved-window-start
                  (set-window-start (get-buffer-window (current-buffer)) saved-window-start)))
            (unless (eq major-mode 'slacko-search-mode)
              (slacko-search-mode))
            (goto-char (point-min)))
          
          ;; Load next page if available, but only after appending results
          ;; For the first page, don't auto-load to avoid race conditions
          (when (and append (< page pages))
            (slacko--load-next-page))))
        
        ;; Switch to buffer AFTER all processing is complete
        (unless append
          (switch-to-buffer buffer)
          (set-window-start (selected-window) (point-min)))))))

(defun slacko--load-next-page ()
  "Load the next page of search results."
  (unless slacko--loading
    (setq slacko--loading t)
    (let* ((next-page (1+ slacko--current-page))
           (response (slacko--make-request slacko--current-query next-page)))
      (setq slacko--loading nil)
      (when response
        (slacko--display-results response t)))))

;;; Major Mode

(defvar slacko-search-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map
    (set-keymap-parent map org-mode-map)
    ;; Add custom keybindings here if needed
    map)
  "Keymap for `slacko-search-mode'.")

(define-derived-mode slacko-search-mode org-mode "Slack-Search"
  "Major mode for displaying Slack search results.

This mode is derived from `org-mode' and displays search results
from Slack in an organized, readable format.  Each result includes
author, channel, timestamp, and message content with proper formatting.

\\{slacko-search-mode-map}"
  (setq buffer-read-only t)
  (slacko-render-setup-font-lock))

;;; Interactive Commands

;;;###autoload
(defun slacko-search (query)
  "Search Slack messages for QUERY.
Display results in an `org-mode' buffer with pagination."
  (interactive "sSearch Slack: ")
  (setq slacko--current-query query
        slacko--current-page 1
        slacko--total-pages nil
        slacko--loading nil)
  ;; (message "Searching Slack for: %s" query)
  (let ((response (slacko--make-request query 1)))
    (when response
      (slacko--display-results response nil))))

(provide 'slacko)
;;; slacko.el ends here
