;;; slack-mrkdwn-tests.el --- tests for slack-mrkdwn.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 19, 2025
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slack-search
;; Package-Requires: ((emacs "30.2"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for Slack mrkdwn to org-mode conversion
;;
;;; Code:

(require 'buttercup)
(require 'slack-mrkdwn)

(describe "slack-mrkdwn--convert-code-blocks"
  (it "converts multiline code block with language"
    (let ((input "```python\nprint('hello')\nprint('world')\n```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src python\nprint('hello')\nprint('world')\n#+end_src")))

  (it "converts multiline code block without language"
    (let ((input "```\nsome code\nmore code\n```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nsome code\nmore code\n#+end_src")))

  (it "converts single-line code block without newlines"
    (let ((input "```export OHI_TOKEN='secret'```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nexport OHI_TOKEN='secret'\n#+end_src")))

  (it "converts code block starting on same line without language"
    (let ((input "```stream | project_types\ndiscovered_schema | data\n```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nstream | project_types\ndiscovered_schema | data\n#+end_src")))

  (it "does not treat first word as language without newline"
    (let ((input "```server: https://example.com:6448```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :not :to-match "#\\+begin_src server")))

  (it "handles multiple code blocks"
    (let* ((input "Text ```code1``` and ```python\ncode2\n``` end")
           (result (slack-mrkdwn--convert-code-blocks input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "code1")
      (expect result :to-match "code2")))

  (it "handles empty code blocks"
    (let ((input "``````"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\n\n#+end_src")))

  (it "trims whitespace from code content"
    (let ((input "```\n  code with spaces  \n```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\ncode with spaces\n#+end_src")))

  (it "preserves internal whitespace and newlines"
    (let ((input "```\nline1\n\nline2\n  indented\n```"))
      (expect (slack-mrkdwn--convert-code-blocks input)
              :to-match "line1\n\nline2\n  indented"))))

(describe "slack-mrkdwn--convert-inline-code"
  (it "converts single backtick to org verbatim"
    (expect (slack-mrkdwn--convert-inline-code "`code`")
            :to-equal "~code~"))

  (it "converts multiple inline code segments"
    (expect (slack-mrkdwn--convert-inline-code "Use `foo` and `bar` here")
            :to-equal "Use ~foo~ and ~bar~ here"))

  (it "handles inline code with special characters"
    (expect (slack-mrkdwn--convert-inline-code "`$variable` and `function()`")
            :to-equal "~$variable~ and ~function()~"))

  (it "does not convert backticks spanning multiple lines"
    (expect (slack-mrkdwn--convert-inline-code "`code\nwith\nnewlines`")
            :to-equal "`code\nwith\nnewlines`"))

  (it "handles text without inline code"
    (expect (slack-mrkdwn--convert-inline-code "regular text")
            :to-equal "regular text"))

  (it "handles empty string"
    (expect (slack-mrkdwn--convert-inline-code "")
            :to-equal "")))

(describe "slack-mrkdwn--convert-links"
  (it "converts Slack link with text"
    (expect (slack-mrkdwn--convert-links "<https://example.com|Example>")
            :to-equal "[[https://example.com][Example]]"))

  (it "converts bare URL"
    (expect (slack-mrkdwn--convert-links "<https://example.com>")
            :to-equal "[[https://example.com]]"))

  (it "converts multiple links"
    (let ((input "See <https://one.com|One> and <https://two.com>"))
      (expect (slack-mrkdwn--convert-links input)
              :to-equal "See [[https://one.com][One]] and [[https://two.com]]")))

  (it "handles links with complex URLs"
    (expect (slack-mrkdwn--convert-links "<https://example.com/path?query=1&foo=bar|Link>")
            :to-equal "[[https://example.com/path?query=1&foo=bar][Link]]"))

  (it "handles text without links"
    (expect (slack-mrkdwn--convert-links "regular text")
            :to-equal "regular text"))

  (it "handles empty string"
    (expect (slack-mrkdwn--convert-links "")
            :to-equal "")))

(describe "slack-mrkdwn--convert-blockquotes"
  (it "converts single blockquote line"
    (expect (slack-mrkdwn--convert-blockquotes "&gt; quoted text")
            :to-equal "│ quoted text"))

  (it "converts blockquote without space after marker"
    (expect (slack-mrkdwn--convert-blockquotes "&gt;quoted text")
            :to-equal "│ quoted text"))

  (it "converts multiline blockquote"
    (expect (slack-mrkdwn--convert-blockquotes "&gt; line one\n&gt; line two")
            :to-equal "│ line one\n│ line two"))

  (it "does not convert &gt; in the middle of a line"
    (expect (slack-mrkdwn--convert-blockquotes "foo &gt; bar")
            :to-equal "foo &gt; bar"))

  (it "handles mixed quoted and unquoted lines"
    (expect (slack-mrkdwn--convert-blockquotes "normal\n&gt; quoted\nnormal again")
            :to-equal "normal\n│ quoted\nnormal again")))

(describe "slack-mrkdwn-to-org"
  (it "handles nil input"
    (expect (slack-mrkdwn-to-org nil)
            :to-be nil))

  (it "handles empty string"
    (expect (slack-mrkdwn-to-org "")
            :to-equal ""))

  (it "converts code blocks before links"
    (let* ((input "```server: <https://example.com:6448>```")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "<https://example.com:6448>")
      (expect result :not :to-match "\\[\\[https://example.com")))

  (it "converts links outside code blocks"
    (let* ((input "See <https://example.com|link> and ```code```")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "\\[\\[https://example.com\\]\\[link\\]\\]")
      (expect result :to-match "#\\+begin_src")))

  (it "handles complex real-world example"
    (let* ((input "If you're using OHI:\n1. <https://devops.com|Console>\n2. Download config\n```server: <https://kubectl.example.com:6448>```\n3. Use `tunnel.sh` script")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "\\[\\[https://devops.com\\]\\[Console\\]\\]")
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "server: <https://kubectl.example.com:6448>")
      (expect result :to-match "~tunnel.sh~")))

  (it "preserves formatting markers (bold, italic)"
    (let ((input "*bold* and _italic_ text"))
      (expect (slack-mrkdwn-to-org input)
              :to-equal "*bold* and _italic_ text")))

  (it "handles mixed inline code and links"
    (let* ((input "Use `command` from <https://example.com|here>")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "~command~")
      (expect result :to-match "\\[\\[https://example.com\\]\\[here\\]\\]")))

  (it "handles multiple code blocks with different content types"
    (let* ((input "First ```python\ncode1\n``` then ```code2``` and <https://link.com|link>")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src python")
      (expect result :to-match "code1")
      (expect result :to-match "code2")
      (expect result :to-match "\\[\\[https://link.com\\]\\[link\\]\\]")))

  (it "handles code blocks with URLs that should not be converted"
    (let* ((input "Config:\n```\nserver: <https://example.com:8080>\n```\nSee also <https://docs.com|docs>")
           (result (slack-mrkdwn-to-org input)))
      ;; URL inside code block should NOT be converted to org link
      (expect result :to-match "server: <https://example.com:8080>")
      (expect result :not :to-match "server: \\[\\[https://example.com:8080\\]\\]")
      ;; URL outside code block SHOULD be converted
      (expect result :to-match "\\[\\[https://docs.com\\]\\[docs\\]\\]")))

  (it "handles edge case with adjacent code and links"
    (let* ((input "```code```<https://link.com>")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "\\[\\[https://link.com\\]\\]")))

  (it "handles nested formatting correctly"
    (let* ((input "Text with `inline code` and *bold `code`* and links <https://example.com>")
           (result (slack-mrkdwn-to-org input)))
      (expect result :to-match "~inline code~")
      (expect result :to-match "~code~")
      (expect result :to-match "\\[\\[https://example.com\\]\\]"))))

;;; slack-mrkdwn-tests.el ends here
