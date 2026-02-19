;;; slacko-tests.el --- tests for slacko -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 19, 2025
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for Slack search functionality
;;
;;; Code:

(require 'buttercup)
(require 'slacko)

(describe "slacko--follow-link"
  (it "constructs correct https URL from slack path"
    (let* ((test-path "//workspace.slack.com/archives/C123/p456")
           (expected-url "https://workspace.slack.com/archives/C123/p456"))
      (spy-on 'browse-url)
      (slacko--follow-link test-path)
      (expect 'browse-url :to-have-been-called-with expected-url)))

  (it "does not close tab when inhibit is nil"
    (let ((slacko-inhibit-redirect-browser-tab nil)
          (test-path "//workspace.slack.com/archives/C123/p456"))
      (spy-on 'browse-url)
      (spy-on 'run-at-time)
      (slacko--follow-link test-path)
      (expect 'run-at-time :not :to-have-been-called)))

  (it "schedules tab close on macOS when inhibit is true"
    (let ((slacko-inhibit-redirect-browser-tab t)
          (system-type 'darwin)
          (test-path "//workspace.slack.com/archives/C123/p456"))
      (spy-on 'browse-url)
      (spy-on 'run-at-time)
      (slacko--follow-link test-path)
      (expect 'run-at-time :to-have-been-called)))

  (it "does not schedule tab close on non-macOS systems"
    (let ((slacko-inhibit-redirect-browser-tab t)
          (system-type 'gnu/linux)
          (test-path "//workspace.slack.com/archives/C123/p456"))
      (spy-on 'browse-url)
      (spy-on 'run-at-time)
      (slacko--follow-link test-path)
      (expect 'run-at-time :not :to-have-been-called))))

(describe "slacko--parse-result"
  (it "parses basic channel message"
    (let* ((match `((username . "john_doe")
                    (user . "U123")
                    (text . "Hello world")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)
                               (is_group . nil)
                               (is_im . nil)
                               (is_mpim . nil)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")
                    (thread_ts . nil)))
           (result (slacko--parse-result match)))
      (expect (plist-get result :author) :to-equal "john_doe")
      (expect (plist-get result :author-id) :to-equal "U123")
      (expect (plist-get result :channel-name) :to-equal "general")
      (expect (plist-get result :channel-id) :to-equal "C456")
      (expect (plist-get result :conversation-type) :to-equal "Channel")
      (expect (plist-get result :host) :to-equal "workspace.slack.com")
      (expect (plist-get result :permalink)
              :to-equal "slack://workspace.slack.com/archives/C456/p1738226435123456")
      (expect (plist-get result :text) :to-equal "Hello world")
      (expect (plist-get result :level) :to-equal 1)))

  (it "identifies DM conversation type"
    (let* ((match `((username . "alice")
                    (user . "U789")
                    (text . "Private message")
                    (channel . ((id . "D123")
                               (name . nil)
                               (is_channel . nil)
                               (is_group . nil)
                               (is_im . t)
                               (is_mpim . nil)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/D123/p1738226435123456")
                    (thread_ts . nil)))
           (result (slacko--parse-result match)))
      (expect (plist-get result :conversation-type) :to-equal "DM")))

  (it "identifies Group DM conversation type"
    (let* ((match `((username . "bob")
                    (user . "U999")
                    (text . "Group chat")
                    (channel . ((id . "G456")
                               (name . "group-dm")
                               (is_channel . nil)
                               (is_group . nil)
                               (is_im . nil)
                               (is_mpim . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/G456/p1738226435123456")
                    (thread_ts . nil)))
           (result (slacko--parse-result match)))
      (expect (plist-get result :conversation-type) :to-equal "Group DM")))

  (it "identifies Private Channel conversation type"
    (let* ((match `((username . "charlie")
                    (user . "U111")
                    (text . "Private channel")
                    (channel . ((id . "G789")
                               (name . "private-channel")
                               (is_channel . nil)
                               (is_group . t)
                               (is_im . nil)
                               (is_mpim . nil)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/G789/p1738226435123456")
                    (thread_ts . nil)))
           (result (slacko--parse-result match)))
      (expect (plist-get result :conversation-type) :to-equal "Private Channel")))

  (it "handles missing username gracefully"
    (let* ((match `((username . nil)
                    (user . "U123")
                    (text . "Message")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")))
           (result (slacko--parse-result match)))
      (expect (plist-get result :author) :to-equal "Unknown")))

  (it "handles missing timestamp gracefully"
    (let* ((match `((username . "dave")
                    (user . "U222")
                    (text . "Message")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . nil)
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")))
           (result (slacko--parse-result match)))
      (expect (plist-get result :ts) :to-equal nil)))

  (it "passes raw text without mrkdwn conversion"
    (let* ((match `((username . "eve")
                    (user . "U333")
                    (text . "Check `code` here")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")))
           (result (slacko--parse-result match)))
      ;; Text should be raw mrkdwn, NOT converted
      (expect (plist-get result :text) :to-equal "Check `code` here")))

  (it "passes raw files from API"
    (let* ((match `((username . "frank")
                    (user . "U444")
                    (text . "See file")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")
                    (files . (((name . "test.png")
                               (permalink . "https://files.slack.com/test.png")
                               (size . 1024)
                               (pretty_type . "PNG"))))))
           (result (slacko--parse-result match)))
      (expect (plist-get result :files) :not :to-be nil)
      (expect (alist-get 'name (car (plist-get result :files)))
              :to-equal "test.png")))

  (it "extracts reactions from API response"
    (let* ((match `((username . "grace")
                    (user . "U555")
                    (text . "Nice!")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")
                    (reactions . (((name . "thumbsup") (count . 3))
                                  ((name . "heart") (count . 1))))))
           (result (slacko--parse-result match)))
      (expect (plist-get result :reactions) :not :to-be nil)
      (expect (length (plist-get result :reactions)) :to-equal 2)))

  (it "extracts share-info for shared messages"
    (let* ((match `((username . "hank")
                    (user . "U666")
                    (text . "")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")
                    (attachments . (((is_share . t)
                                     (author_name . "original_user")
                                     (author_id . "U999")
                                     (channel_id . "C789")
                                     (from_url . "https://workspace.slack.com/archives/C789/p1738000000000000")
                                     (text . "Original message"))))))
           (result (slacko--parse-result match)))
      (expect (plist-get result :share-info) :not :to-be nil)
      (expect (plist-get (plist-get result :share-info) :author-name)
              :to-equal "original_user")
      ;; Falls back to attachment text when main text is empty
      (expect (plist-get result :text) :to-equal "Original message"))))

(describe "slacko--display-results"
  (it "handles successful response with matches"
    (let* ((response `((ok . t)
                       (messages . ((matches . (((username . "test")
                                                  (user . "U123")
                                                  (text . "test message")
                                                  (channel . ((id . "C456")
                                                             (name . "general")
                                                             (is_channel . t)))
                                                  (ts . "1738226435.123456")
                                                  (permalink . "https://workspace.slack.com/archives/C456/p123"))))
                                    (total . 1)
                                    (paging . ((page . 1)
                                              (pages . 1)))))
                       (query . "test"))))
      ;; Stub mention resolution to avoid API calls
      (spy-on 'slacko-render-resolve-user-mentions :and-call-fake
              (lambda (_host text) text))
      (spy-on 'slacko-render-resolve-channel-mentions :and-call-fake
              (lambda (_host text) text))
      (slacko--display-results response)
      (with-current-buffer slacko-search-buffer-name
        (let ((content (buffer-string)))
          (expect content :to-match "#\\+TITLE: Slack Search Results")
          (expect content :to-match "Total results: 1")
          (expect content :to-match "test message")))))

  (it "handles zero total gracefully"
    (let* ((response `((ok . t)
                       (messages . ((matches . nil)
                                    (total . 0)
                                    (paging . ((page . 1)
                                              (pages . 1))))))))
      (slacko--display-results response)
      (with-current-buffer slacko-search-buffer-name
        (let ((content (buffer-string)))
          (expect content :to-match "Total results: 0")))))

  (it "handles error response"
    (let* ((response `((ok . nil)
                       (error . "invalid_auth"))))
      (spy-on 'message)
      (slacko--display-results response)
      (expect 'message :to-have-been-called-with
              "Slack search failed: %s" "invalid_auth"))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-tests.el ends here
