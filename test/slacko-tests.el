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
      (expect (plist-get result :user-id) :to-equal "U123")
      (expect (plist-get result :channel) :to-equal "general")
      (expect (plist-get result :channel-id) :to-equal "C456")
      (expect (plist-get result :conversation-type) :to-equal "Channel")
      (expect (plist-get result :workspace-url) :to-equal "https://workspace.slack.com")
      (expect (plist-get result :permalink) :to-equal "https://workspace.slack.com/archives/C456/p1738226435123456")))

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
      (expect (plist-get result :timestamp) :to-equal "unknown date")))

  (it "converts text using slacko-mrkdwn-to-org"
    (let* ((match `((username . "eve")
                    (user . "U333")
                    (text . "Check `code` here")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")))
           (result (slacko--parse-result match)))
      (expect (plist-get result :text) :to-match "~code~")))

  (it "identifies thread messages"
    (let* ((match `((username . "frank")
                    (user . "U444")
                    (text . "Reply")
                    (channel . ((id . "C456")
                               (name . "general")
                               (is_channel . t)))
                    (ts . "1738226435.123456")
                    (permalink . "https://workspace.slack.com/archives/C456/p1738226435123456")
                    (thread_ts . "1738226400.000000")))
           (result (slacko--parse-result match)))
      (expect (plist-get result :thread) :to-equal "Thread"))))

(describe "slacko--insert-result"
  (it "inserts formatted result with all links"
    (let* ((result '(:author "john_doe"
                     :user-id "U123"
                     :workspace-url "https://workspace.slack.com"
                     :channel "general"
                     :channel-id "C456"
                     :conversation-type "Channel"
                     :timestamp "Jan 30 at 10:00 AM"
                     :permalink "https://workspace.slack.com/archives/C456/p123"
                     :thread ""
                     :text "Hello world")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Check for author link
          (expect output :to-match "\\[\\[slack://workspace.slack.com/team/U123\\]\\[john_doe\\]\\]")
          ;; Check for channel link
          (expect output :to-match "\\[\\[slack://workspace.slack.com/archives/C456\\]\\[#general\\]\\]")
          ;; Check for permalink
          (expect output :to-match "\\[\\[slack://workspace.slack.com/archives/C456/p123\\]\\[Jan 30 at 10:00 AM\\]\\]")
          ;; Check for text content
          (expect output :to-match "Hello world")))))

  (it "uses conversation type for DM links"
    (let* ((result '(:author "alice"
                     :user-id "U789"
                     :workspace-url "https://workspace.slack.com"
                     :channel "unknown"
                     :channel-id "D123"
                     :conversation-type "DM"
                     :timestamp "Jan 30 at 11:00 AM"
                     :permalink "https://workspace.slack.com/archives/D123/p456"
                     :thread ""
                     :text "Private message")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Check that DM is used instead of channel name
          (expect output :to-match "\\[\\[slack://workspace.slack.com/archives/D123\\]\\[DM\\]\\]")
          (expect output :not :to-match "#unknown")))))

  (it "handles missing user-id gracefully"
    (let* ((result '(:author "bob"
                     :user-id nil
                     :workspace-url "https://workspace.slack.com"
                     :channel "general"
                     :channel-id "C456"
                     :conversation-type "Channel"
                     :timestamp "Jan 30 at 12:00 PM"
                     :permalink "https://workspace.slack.com/archives/C456/p789"
                     :thread ""
                     :text "Message")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Should use plain author name without link
          (expect output :to-match "\\* bob |")
          (expect output :not :to-match "\\[\\[slack://.*team")))))

  (it "handles missing channel-id gracefully"
    (let* ((result '(:author "charlie"
                     :user-id "U111"
                     :workspace-url "https://workspace.slack.com"
                     :channel "general"
                     :channel-id nil
                     :conversation-type "Channel"
                     :timestamp "Jan 30 at 01:00 PM"
                     :permalink "https://workspace.slack.com/archives/C456/p999"
                     :thread ""
                     :text "Message")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Should use plain channel name without link (but permalink still has archives)
          (expect output :to-match "#general")
          ;; Check that the channel part is NOT a link (just plain text)
          (expect output :to-match "| #general |")))))

  (it "formats org-mode heading correctly"
    (let* ((result '(:author "dave"
                     :user-id "U222"
                     :workspace-url "https://workspace.slack.com"
                     :channel "test"
                     :channel-id "C789"
                     :conversation-type "Channel"
                     :timestamp "Jan 30 at 02:00 PM"
                     :permalink "https://workspace.slack.com/archives/C789/p111"
                     :thread ""
                     :text "Test message")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Check org heading starts with *
          (expect output :to-match "^\\*")
          ;; Check proper spacing and separators
          (expect output :to-match "\\* .* | .* | \\[\\[")))))

  (it "preserves formatted text content"
    (let* ((result '(:author "eve"
                     :user-id "U333"
                     :workspace-url "https://workspace.slack.com"
                     :channel "dev"
                     :channel-id "C999"
                     :conversation-type "Channel"
                     :timestamp "Jan 30 at 03:00 PM"
                     :permalink "https://workspace.slack.com/archives/C999/p222"
                     :thread ""
                     :text "Code: ~npm install~\n\n#+begin_src\nfunction test() {}\n#+end_src")))
      (with-temp-buffer
        (slacko--insert-result result)
        (let ((output (buffer-string)))
          ;; Check that formatted content is preserved
          (expect output :to-match "~npm install~")
          (expect output :to-match "#\\+begin_src"))))))

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
