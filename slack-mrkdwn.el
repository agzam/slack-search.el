;;; slack-mrkdwn.el --- Convert Slack mrkdwn to org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 19, 2025
;; Modified: October 19, 2025
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
;;  Convert Slack's mrkdwn format to org-mode markup.
;;  
;;  Slack uses a variant of markdown called "mrkdwn" which has some
;;  non-standard syntax like <url|text> for links and <@USER_ID> for mentions.
;;
;;; Code:

(require 'subr-x)

;;; Conversion Functions

(defun slack-mrkdwn--convert-code-blocks (text)
  "Convert Slack code blocks to org src blocks in TEXT.
Handles blocks with or without language identifiers."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match ``` followed by optional lang (only if followed by newline), then code, then ```
    ;; Two patterns: ```lang\ncode``` or ```code```
    (while (re-search-forward "```\\(?:\\([a-zA-Z0-9]+\\)\n\\)?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (lang (match-string-no-properties 1))  ; Only captured if followed by newline
             (code (match-string-no-properties 2))
             ;; Trim leading/trailing whitespace from code
             (trimmed-code (string-trim code))
             (replacement (if (and lang (not (string-empty-p lang)))
                              (format "#+begin_src %s\n%s\n#+end_src" lang trimmed-code)
                            (format "#+begin_src\n%s\n#+end_src" trimmed-code))))
        (delete-region match-start match-end)
        (goto-char match-start)
        (insert replacement)))
    (buffer-string)))

(defun slack-mrkdwn--convert-inline-code (text)
  "Convert inline code (`code`) to org verbatim (~code~) in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match `code` but not inside code blocks (those should already be converted)
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (replace-match "~\\1~" t))
    (buffer-string)))

(defun slack-mrkdwn--convert-links (text)
  "Convert Slack links to org links in TEXT.
Converts both <url|text> and bare <url> formats."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Convert <url|text> to [[url][text]]
    (while (re-search-forward "<\\([^|>]+\\)|\\([^>]+\\)>" nil t)
      (replace-match "[[\\1][\\2]]" t))
    
    (goto-char (point-min))
    ;; Convert bare <url> to [[url]]
    (while (re-search-forward "<\\(https?://[^>]+\\)>" nil t)
      (replace-match "[[\\1]]" t))
    (buffer-string)))

(defun slack-mrkdwn-to-org (text)
  "Convert Slack mrkdwn TEXT to `org-mode' format.

This handles:
- Code blocks: ```lang\\ncode\\n``` → #+begin_src lang...#+end_src
- Inline code: `code` → ~code~
- Links: <url|text> → [[url][text]]
- Bare URLs: <url> → [[url]]

Not yet implemented:
- User mentions: <@USER_ID>
- Channel mentions: <#CHANNEL_ID>
- Special formatting (these mostly work as-is in `org-mode')
  - Bold: *text*
  - Italic: _text_
  - Strikethrough: ~text~"
  (when (stringp text)
    (let* ((protected-blocks '())
           (counter 0)
           (text-with-placeholders text))
      
      ;; First, extract and protect code blocks with placeholders
      (setq text-with-placeholders
            (replace-regexp-in-string
             "```\\(?:[a-zA-Z0-9]+\n\\)?\\(?:.\\|\n\\)*?```"
             (lambda (match)
               (let ((placeholder (format "<<<CODE-BLOCK-%d>>>" counter)))
                 (push (cons placeholder match) protected-blocks)
                 (setq counter (1+ counter))
                 placeholder))
             text-with-placeholders))
      
      ;; Process links (now code blocks are protected)
      (setq text-with-placeholders (slack-mrkdwn--convert-links text-with-placeholders))
      
      ;; Restore and convert code blocks
      (dolist (pair protected-blocks)
        (setq text-with-placeholders
              (replace-regexp-in-string
               (regexp-quote (car pair))
               (slack-mrkdwn--convert-code-blocks (cdr pair))
               text-with-placeholders
               t t)))
      
      ;; Finally convert inline code
      (slack-mrkdwn--convert-inline-code text-with-placeholders))))

(provide 'slack-mrkdwn)
;;; slack-mrkdwn.el ends here
