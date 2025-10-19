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

;;; Internal Variables

(defvar slack-search--current-query nil
  "The current search query.")

(defvar slack-search--current-page 1
  "Current page number for pagination.")

(defvar slack-search--total-pages nil
  "Total number of pages available.")

(defvar slack-search--loading nil
  "Flag indicating if a search is currently loading.")

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

(defun slack-search--parse-result (match workspace-url)
  "Parse a single search result MATCH into display format.
WORKSPACE-URL is the base Slack workspace URL (unused, kept for compatibility)."
  (let* ((username (alist-get 'username match))
         (text (alist-get 'text match))
         (channel (alist-get 'channel match))
         (channel-name (if channel
                           (alist-get 'name channel)
                         "unknown"))
         (ts (alist-get 'ts match))
         (timestamp (when (and ts (stringp ts))
                      (condition-case nil
                          (format-time-string "%b %d at %I:%M %p"
                                              (seconds-to-time (string-to-number ts)))
                        (error "Unknown date"))))
         ;; Use the permalink from the API response directly
         (permalink (alist-get 'permalink match))
         (thread-ts (alist-get 'thread_ts match))
         (thread-info (if thread-ts "Thread" "")))
    (list :author (if (stringp username) username "Unknown")
          :channel (if (stringp channel-name) channel-name "unknown")
          :timestamp (if (stringp timestamp) timestamp "unknown date")
          :permalink (if (stringp permalink) permalink "")
          :thread thread-info
          :text (if (stringp text) text ""))))

(defun slack-search--insert-result (result)
  "Insert a single RESULT into the current buffer."
  (let* ((author (plist-get result :author))
         (channel (plist-get result :channel))
         (timestamp (plist-get result :timestamp))
         (permalink (plist-get result :permalink))
         (thread (plist-get result :thread))
         (text (plist-get result :text))
         ;; Ensure all values are strings
         (author-str (if (stringp author) author "Unknown"))
         (channel-str (if (stringp channel) channel "unknown"))
         (timestamp-str (if (stringp timestamp) timestamp "unknown date"))
         (permalink-str (if (stringp permalink) permalink "#"))
         (text-str (if (stringp text) text "")))
    (insert (format "* %s | #%s | [[%s][%s]]\n\n  %s\n\n"
                    author-str
                    channel-str
                    permalink-str
                    timestamp-str
                    text-str))))

(defun slack-search--display-results (response &optional append)
  "Display search results from RESPONSE in 'org-mode' buffer.
If APPEND is non-nil, append to existing results."
  (let* ((ok (alist-get 'ok response))
         (query-str (alist-get 'query response))
         (messages-data (alist-get 'messages response))
         (matches (alist-get 'matches messages-data))
         (total (alist-get 'total messages-data))
         (paging (alist-get 'paging messages-data))
         (page (alist-get 'page paging))
         (pages (alist-get 'pages paging))
         ;; Extract workspace URL from the first match's permalink if available
         (first-match (car matches))
         (permalink-api (when first-match (alist-get 'permalink first-match)))
         (workspace-url (when permalink-api
                          (and (string-match "\\(https://[^/]+\\)" permalink-api)
                               (match-string 1 permalink-api)))))
    
    (if (not ok)
        (message "Slack search failed: %s" (alist-get 'error response))
      
      (with-current-buffer (get-buffer-create slack-search-buffer-name)
        (let ((saved-point (when append (point)))
              (saved-window-start (when append
                                    (and (get-buffer-window (current-buffer))
                                         (window-start (get-buffer-window (current-buffer)))))))
          
          (unless append
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "#+TITLE: Slack Search Results for: %s\n" slack-search--current-query))
              (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert (format "Total results: %d\n\n" total))))
          
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (dolist (match matches)
              (slack-search--insert-result (slack-search--parse-result match workspace-url))))
          
          (setq slack-search--current-page page
                slack-search--total-pages pages)
          
          ;; Restore position when appending, otherwise go to top
          (if append
              (progn
                (when saved-point (goto-char saved-point))
                (when saved-window-start
                  (set-window-start (get-buffer-window (current-buffer)) saved-window-start)))
            (org-mode)
            (switch-to-buffer (current-buffer))
            (goto-char (point-min))
            (set-window-start (selected-window) (point-min)))
          
          ;; Load next page if available
          (when (< page pages)
            (slack-search--load-next-page)))))))

(defun slack-search--load-next-page ()
  "Load the next page of search results asynchronously."
  (unless slack-search--loading
    (setq slack-search--loading t)
    (let ((next-page (1+ slack-search--current-page)))
      (message "Loading page %d of %d..." next-page slack-search--total-pages)
      (slack-search--make-request
       slack-search--current-query
       next-page
       (lambda (response)
         (setq slack-search--loading nil)
         (slack-search--display-results response t)
         (message "Loaded page %d of %d" slack-search--current-page slack-search--total-pages))))))

;;; Interactive Commands

;;;###autoload
(defun slack-search (query)
  "Search Slack messages for QUERY.
Display results in an 'org-mode' buffer with pagination."
  (interactive "sSearch Slack: ")
  (setq slack-search--current-query query
        slack-search--current-page 1
        slack-search--total-pages nil
        slack-search--loading nil)
  (message "Searching Slack for: %s" query)
  (slack-search--make-request
   query
   1
   (lambda (response)
     (slack-search--display-results response nil))))

(provide 'slack-search)
;;; slack-search.el ends here
