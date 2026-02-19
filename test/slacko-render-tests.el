;;; slacko-render-tests.el --- tests for slacko-render -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2026
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for the unified message rendering module.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-render)

;;; Timestamp

(describe "slacko-render-format-timestamp"
  (it "formats a valid timestamp"
    (let ((slacko-render-timestamp-format "%Y-%m-%d %H:%M"))
      (expect (slacko-render-format-timestamp "1738226435.123456")
              :to-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}$")))

  (it "returns nil for nil input"
    (expect (slacko-render-format-timestamp nil) :to-be nil))

  (it "returns nil for non-string input"
    (expect (slacko-render-format-timestamp 12345) :to-be nil))

  (it "respects custom format"
    (let ((slacko-render-timestamp-format "%b %d at %I:%M %p"))
      (expect (slacko-render-format-timestamp "1738226435.123456")
              :to-match "at"))))

;;; Author Link

(describe "slacko-render--build-author-link"
  (it "builds org link when host and author-id available"
    (let ((link (slacko-render--build-author-link
                 "workspace.slack.com" "John" "U123")))
      (expect link :to-equal
              "[[slack://workspace.slack.com/team/U123][John]]")))

  (it "returns plain name when host is nil"
    (expect (slacko-render--build-author-link nil "John" "U123")
            :to-equal "John"))

  (it "returns plain name when author-id is nil"
    (expect (slacko-render--build-author-link "host" "John" nil)
            :to-equal "John")))

;;; Channel Link

(describe "slacko-render--build-channel-link"
  (it "builds channel link for regular channel"
    (let ((link (slacko-render--build-channel-link
                 "workspace.slack.com" "general" "C456" "Channel")))
      (expect link :to-equal
              "[[slack://workspace.slack.com/archives/C456][#general]]")))

  (it "uses conversation type for DMs"
    (let ((link (slacko-render--build-channel-link
                 "workspace.slack.com" nil "D123" "DM")))
      (expect link :to-equal
              "[[slack://workspace.slack.com/archives/D123][DM]]")))

  (it "uses conversation type for Group DM"
    (let ((link (slacko-render--build-channel-link
                 "workspace.slack.com" "group-dm" "G456" "Group DM")))
      (expect link :to-equal
              "[[slack://workspace.slack.com/archives/G456][Group DM]]")))

  (it "returns nil when host is missing"
    (expect (slacko-render--build-channel-link nil "general" "C456" "Channel")
            :to-be nil))

  (it "returns nil when channel-id is missing"
    (expect (slacko-render--build-channel-link "host" "general" nil "Channel")
            :to-be nil)))

;;; File Size

(describe "slacko-render--format-file-size"
  (it "formats bytes"
    (expect (slacko-render--format-file-size 512) :to-equal "512 B"))

  (it "formats kilobytes"
    (expect (slacko-render--format-file-size 2048) :to-equal "2.0 KB"))

  (it "formats megabytes"
    (expect (slacko-render--format-file-size (* 1024 1024 3))
            :to-equal "3.0 MB"))

  (it "returns empty for nil"
    (expect (slacko-render--format-file-size nil) :to-equal "")))

;;; User Mention Resolution

(describe "slacko-render-resolve-user-mentions"
  (it "returns text unchanged when host is nil"
    (expect (slacko-render-resolve-user-mentions nil "Hello <@U123>!")
            :to-equal "Hello <@U123>!"))

  (it "returns text unchanged when no mentions"
    (expect (slacko-render-resolve-user-mentions "host" "Hello world")
            :to-equal "Hello world"))

  (it "returns nil unchanged"
    (expect (slacko-render-resolve-user-mentions "host" nil)
            :to-be nil))

  (it "resolves mentions using cache"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (puthash "host:U123" "Alice" slacko-render--user-cache)
      (expect (slacko-render-resolve-user-mentions "host" "Hi <@U123>!")
              :to-equal "Hi @Alice!")))

  (it "resolves multiple mentions"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (puthash "host:U123" "Alice" slacko-render--user-cache)
      (puthash "host:U456" "Bob" slacko-render--user-cache)
      (expect (slacko-render-resolve-user-mentions
               "host" "<@U123> and <@U456>")
              :to-equal "@Alice and @Bob"))))

;;; Channel Mention Resolution

(describe "slacko-render-resolve-channel-mentions"
  (it "returns text unchanged when host is nil"
    (expect (slacko-render-resolve-channel-mentions
             nil "See <#C123|general>")
            :to-equal "See <#C123|general>"))

  (it "returns text unchanged when no mentions"
    (expect (slacko-render-resolve-channel-mentions "host" "Hello world")
            :to-equal "Hello world"))

  (it "uses inline name when provided"
    (expect (slacko-render-resolve-channel-mentions
             "host" "See <#C123|general>")
            :to-equal
            "See [[slack://host/archives/C123][#general]]"))

  (it "resolves empty name from cache"
    (let ((slacko-render--channel-cache (make-hash-table :test 'equal)))
      (puthash "host:C123" "random" slacko-render--channel-cache)
      (expect (slacko-render-resolve-channel-mentions
               "host" "See <#C123|>")
              :to-equal
              "See [[slack://host/archives/C123][#random]]"))))

;;; Message Rendering

(describe "slacko-render-message"
  :var (slacko-render-timestamp-format)
  (before-each
    (setq slacko-render-timestamp-format "%Y-%m-%d %H:%M")
    ;; Stub mention resolution to avoid API calls
    (spy-on 'slacko-render-resolve-user-mentions :and-call-fake
            (lambda (_host text) text))
    (spy-on 'slacko-render-resolve-channel-mentions :and-call-fake
            (lambda (_host text) text)))

  (it "renders basic message with channel"
    (with-temp-buffer
      (slacko-render-message
       '(:author "john_doe"
         :author-id "U123"
         :text "Hello world"
         :ts "1738226435.123456"
         :permalink "slack://workspace.slack.com/archives/C456/p123"
         :level 1
         :host "workspace.slack.com"
         :channel-name "general"
         :channel-id "C456"
         :conversation-type "Channel"))
      (let ((output (buffer-string)))
        ;; Author link
        (expect output :to-match
                "\\[\\[slack://workspace.slack.com/team/U123\\]\\[john_doe\\]\\]")
        ;; Channel link
        (expect output :to-match
                "\\[\\[slack://workspace.slack.com/archives/C456\\]\\[#general\\]\\]")
        ;; Permalink with timestamp
        (expect output :to-match
                "\\[\\[slack://workspace.slack.com/archives/C456/p123\\]\\[")
        ;; Text body
        (expect output :to-match "Hello world"))))

  (it "renders message without channel"
    (with-temp-buffer
      (slacko-render-message
       '(:author "john_doe"
         :author-id "U123"
         :text "Thread reply"
         :ts "1738226435.123456"
         :permalink "slack://host/archives/C456/p123"
         :level 2
         :host "host"))
      (let ((output (buffer-string)))
        ;; Level 2 heading
        (expect output :to-match "^\\*\\* ")
        ;; No channel link
        (expect output :not :to-match "#general")
        ;; Text
        (expect output :to-match "Thread reply"))))

  (it "uses DM for conversation type in channel link"
    (with-temp-buffer
      (slacko-render-message
       '(:author "alice"
         :author-id "U789"
         :text "Private message"
         :ts "1738226435.123456"
         :permalink "slack://workspace.slack.com/archives/D123/p456"
         :level 1
         :host "workspace.slack.com"
         :channel-name nil
         :channel-id "D123"
         :conversation-type "DM"))
      (let ((output (buffer-string)))
        (expect output :to-match
                "\\[\\[slack://workspace.slack.com/archives/D123\\]\\[DM\\]\\]"))))

  (it "renders reactions"
    (with-temp-buffer
      (slacko-render-message
       `(:author "bob"
         :text "Nice!"
         :ts "1738226435.123456"
         :permalink "slack://host/archives/C456/p123"
         :level 1
         :host "host"
         :reactions (((name . "thumbsup") (count . 3))
                     ((name . "heart") (count . 1)))))
      (let ((output (buffer-string)))
        ;; count is preceded by a zero-width space (\u200B), not a regular space
        (expect output :to-match ":thumbsup:\u200B3")
        (expect output :to-match ":heart:\u200B1"))))

  (it "renders file attachments as links"
    (with-temp-buffer
      (let ((slacko-render-inline-images nil))
        (slacko-render-message
         `(:author "charlie"
           :text "See attached"
           :ts "1738226435.123456"
           :permalink "slack://host/archives/C456/p123"
           :level 1
           :host "host"
           :files (((name . "report.pdf")
                    (url_private . "https://files.slack.com/report.pdf")
                    (pretty_type . "PDF")
                    (size . 2048))))))
      (let ((output (buffer-string)))
        (expect output :to-match "report\\.pdf")
        (expect output :to-match "2\\.0 KB")
        (expect output :to-match "PDF"))))

  (it "handles missing author gracefully"
    (with-temp-buffer
      (slacko-render-message
       '(:text "Message"
         :ts "1738226435.123456"
         :permalink "slack://host/p123"
         :level 1))
      (let ((output (buffer-string)))
        (expect output :to-match "Unknown"))))

  (it "converts mrkdwn to org"
    (with-temp-buffer
      (slacko-render-message
       '(:author "dave"
         :text "Check `code` here"
         :ts "1738226435.123456"
         :permalink "slack://host/p123"
         :level 1))
      (let ((output (buffer-string)))
        (expect output :to-match "~code~"))))

  (it "renders org heading at correct level"
    (with-temp-buffer
      (slacko-render-message
       '(:author "eve"
         :text "Test"
         :ts "1738226435.123456"
         :permalink "slack://host/p123"
         :level 3
         :host "host"))
      (let ((output (buffer-string)))
        (expect output :to-match "^\\*\\*\\* ")))))

(describe "slacko-render--insert-reactions"
  (it "inserts formatted reaction line"
    (with-temp-buffer
      (slacko-render--insert-reactions
       '(((name . "fire") (count . 5))
         ((name . "100") (count . 2))))
      ;; count is preceded by a zero-width space (\u200B) with display properties
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (expect text :to-equal ":fire:\u200B5  :100:\u200B2\n"))))

  (it "does nothing for nil reactions"
    (with-temp-buffer
      (slacko-render--insert-reactions nil)
      (expect (buffer-string) :to-equal ""))))

(describe "slacko-render--insert-files"
  (it "inserts file links with size and type"
    (with-temp-buffer
      (let ((slacko-render-inline-images nil))
        (slacko-render--insert-files
         '(((name . "doc.pdf")
            (url_private . "https://files.slack.com/doc.pdf")
            (pretty_type . "PDF")
            (size . 512000)))
         nil))
      (let ((output (buffer-string)))
        (expect output :to-match "doc\\.pdf")
        (expect output :to-match "500\\.0 KB")
        (expect output :to-match "PDF"))))

  (it "handles files without size"
    (with-temp-buffer
      (let ((slacko-render-inline-images nil))
        (slacko-render--insert-files
         '(((name . "file.txt")
            (url_private . "https://files.slack.com/file.txt")
            (pretty_type . "Text")))
         nil))
      (let ((output (buffer-string)))
        (expect output :to-match "file\\.txt")
        (expect output :to-match "(Text)")))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-render-tests.el ends here
