;;; slacko-emoji-tests.el --- tests for slacko-emoji -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2025
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for slacko-emoji minor mode
;;
;;; Code:

(require 'buttercup)
(require 'slacko-emoji)

(describe "slacko-emoji--shortcode-to-unicode"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "returns unicode for known emoji"
    (expect (slacko-emoji--shortcode-to-unicode ":smile:") :to-equal "😄"))

  (it "returns unicode for thumbsup"
    (expect (slacko-emoji--shortcode-to-unicode ":+1:") :to-equal "👍"))

  (it "returns unicode for heart"
    (expect (slacko-emoji--shortcode-to-unicode ":heart:") :to-equal "❤"))

  (it "returns nil for unknown shortcode"
    (expect (slacko-emoji--shortcode-to-unicode ":not_a_real_emoji_xyz:") :to-be nil))

  (it "returns nil for custom/workspace emoji"
    (expect (slacko-emoji--shortcode-to-unicode ":my_company_logo:") :to-be nil)))

(describe "slacko-emoji--emojify-region"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "places overlays on shortcodes in a buffer"
    (with-temp-buffer
      (insert "Hello :smile: world :fire: end")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let ((ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                             (overlays-in (point-min) (point-max)))))
        (expect (length ovs) :to-equal 2)
        ;; check display properties
        (let ((sorted (sort ovs (lambda (a b) (< (overlay-start a) (overlay-start b))))))
          (expect (overlay-get (nth 0 sorted) 'display) :to-equal "😄")
          (expect (overlay-get (nth 1 sorted) 'display) :to-equal "🔥")))))

  (it "skips unknown shortcodes"
    (with-temp-buffer
      (insert "Hello :nonexistent_emoji: world")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let ((ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                             (overlays-in (point-min) (point-max)))))
        (expect (length ovs) :to-equal 0))))

  (it "does not double-overlay the same shortcode"
    (with-temp-buffer
      (insert ":smile:")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let ((ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                             (overlays-in (point-min) (point-max)))))
        (expect (length ovs) :to-equal 1))))

  (it "stores the shortcode on the overlay"
    (with-temp-buffer
      (insert ":thinking_face:")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let ((ov (car (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                                 (overlays-in (point-min) (point-max))))))
        (expect ov :not :to-be nil)
        (expect (overlay-get ov 'slacko-emoji-shortcode) :to-equal ":thinking_face:")))))

(describe "slacko-emoji--remove-overlays"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "removes all emoji overlays"
    (with-temp-buffer
      (insert ":smile: :fire: :heart:")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (expect (length (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                                  (overlays-in (point-min) (point-max))))
              :to-be-greater-than 0)
      (slacko-emoji--remove-overlays)
      (expect (length (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                                  (overlays-in (point-min) (point-max))))
              :to-equal 0))))

(describe "slacko-emoji-mode"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "enables and places overlays"
    (with-temp-buffer
      (insert ":smile: hello :fire:")
      (slacko-emoji-mode 1)
      (let ((ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                             (overlays-in (point-min) (point-max)))))
        (expect (length ovs) :to-equal 2))
      (slacko-emoji-mode -1)))

  (it "disables and removes overlays"
    (with-temp-buffer
      (insert ":smile: hello :fire:")
      (slacko-emoji-mode 1)
      (slacko-emoji-mode -1)
      (let ((ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                             (overlays-in (point-min) (point-max)))))
        (expect (length ovs) :to-equal 0))))

  (it "adds post-command-hook when enabled"
    (with-temp-buffer
      (slacko-emoji-mode 1)
      (expect (memq #'slacko-emoji--post-command post-command-hook) :not :to-be nil)
      (slacko-emoji-mode -1)
      (expect (memq #'slacko-emoji--post-command post-command-hook) :to-be nil))))

(describe "slacko-emoji reaction counts"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "renders count as superscript overlay covering space+digits"
    (with-temp-buffer
      (insert ":+1: 3")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let* ((all-ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                                  (overlays-in (point-min) (point-max))))
             (emoji-ovs (seq-filter (lambda (ov) (overlay-get ov 'slacko-emoji-shortcode)) all-ovs))
             (count-ovs (seq-filter (lambda (ov) (overlay-get ov 'slacko-emoji-count)) all-ovs)))
        (expect (length emoji-ovs) :to-equal 1)
        (expect (length count-ovs) :to-equal 1)
        (expect (overlay-get (car emoji-ovs) 'display) :to-equal "👍")
        ;; count overlay covers " 3" (space + digits)
        (let ((cov (car count-ovs)))
          (expect (buffer-substring (overlay-start cov) (overlay-end cov))
                  :to-equal " 3")))))

  (it "handles multiple reactions with counts"
    (with-temp-buffer
      (insert ":+1: 3  :heart: 1  :fire: 12")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let* ((all-ovs (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
                                  (overlays-in (point-min) (point-max))))
             (count-ovs (seq-filter (lambda (ov) (overlay-get ov 'slacko-emoji-count)) all-ovs)))
        (expect (length count-ovs) :to-equal 3))))

  (it "does not create count overlay when no count follows"
    (with-temp-buffer
      (insert ":smile: hello")
      (slacko-emoji--emojify-region (point-min) (point-max))
      (let ((count-ovs (seq-filter (lambda (ov) (overlay-get ov 'slacko-emoji-count))
                                   (overlays-in (point-min) (point-max)))))
        (expect (length count-ovs) :to-equal 0)))))

(describe "slacko-emoji reveal/conceal"
  (before-all
    (unless (and (boundp 'emojify-emojis)
                 (hash-table-p emojify-emojis)
                 (> (hash-table-count emojify-emojis) 0))
      (emojify-create-emojify-emojis)))

  (it "reveals shortcode when overlay is at point"
    (with-temp-buffer
      (insert ":smile:")
      (slacko-emoji-mode 1)
      (goto-char (point-min))
      (let ((ov (slacko-emoji--overlay-at-point)))
        (expect ov :not :to-be nil)
        (slacko-emoji--reveal ov)
        (expect (overlay-get ov 'display) :to-be nil))
      (slacko-emoji-mode -1)))

  (it "conceals shortcode restoring the emoji display"
    (with-temp-buffer
      (insert ":smile:")
      (slacko-emoji-mode 1)
      (goto-char (point-min))
      (let ((ov (slacko-emoji--overlay-at-point)))
        (slacko-emoji--reveal ov)
        (slacko-emoji--conceal ov)
        (expect (overlay-get ov 'display) :to-equal "😄"))
      (slacko-emoji-mode -1))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-emoji-tests.el ends here
