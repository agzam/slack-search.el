;;; slacko-emoji.el --- Emoji overlay support for slacko -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2025
;; Keywords: tools
;; Homepage: https://github.com/agzam/slacko
;; Package-Requires: ((emacs "30.2") (emojify "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Buffer-local minor mode that overlays Slack-style shortcodes like :smile:
;; with their Unicode emoji equivalents (e.g. 😄), using the `emojify'
;; package's shortcode-to-unicode mapping.
;;
;; The underlying buffer text is never mutated - overlays with `display'
;; properties are used instead.  When the cursor is on an emoji overlay,
;; the original shortcode is revealed (org-appear style).
;;
;;; Code:

(require 'emojify)

(defgroup slacko-emoji nil
  "Emoji rendering for slacko buffers."
  :group 'slacko
  :prefix "slacko-emoji-")

(defvar-local slacko-emoji--revealed-overlays nil
  "List of overlays currently revealed (showing shortcode text).")

(defface slacko-emoji-count
  '((t :height 0.7 :inherit default))
  "Face for reaction count superscripts next to emoji."
  :group 'slacko-emoji)

;;; Core lookup

(defun slacko-emoji--shortcode-to-unicode (shortcode)
  "Look up SHORTCODE (e.g. \":smile:\") and return its Unicode string, or nil."
  (when-let* ((entry (gethash shortcode emojify-emojis)))
    (gethash "unicode" entry)))

;;; Overlay management

(defun slacko-emoji--make-overlay (beg end unicode shortcode)
  "Create an emoji overlay from BEG to END showing UNICODE for SHORTCODE."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'category 'slacko-emoji)
    (overlay-put ov 'display unicode)
    (overlay-put ov 'slacko-emoji-shortcode shortcode)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'help-echo shortcode)
    ov))

(defun slacko-emoji--make-count-overlay (beg end count-str)
  "Create a superscript overlay from BEG to END showing COUNT-STR.
BEG should include the leading space so it gets replaced by a thin space."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'category 'slacko-emoji)
    (overlay-put ov 'display
                 (propertize (concat "\u200B" count-str) ;; zero-width space + digits
                             'face 'slacko-emoji-count
                             'display '(raise 0.3)))
    (overlay-put ov 'slacko-emoji-count t)
    (overlay-put ov 'evaporate t)
    ov))

(defun slacko-emoji--emojify-region (beg end)
  "Scan region from BEG to END for :shortcode: patterns and place emoji overlays.
When a shortcode is followed by a space and a number (reaction count),
the count is rendered as superscript."
  (when (hash-table-p emojify-emojis)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\(:[a-z0-9_+-]+:\\)\\( \\([0-9]+\\)\\)?" end t)
        (let* ((shortcode (match-string 1))
               (mb (match-beginning 1))
               (me (match-end 1))
               (unicode (slacko-emoji--shortcode-to-unicode shortcode)))
          (when (and unicode
                     ;; don't double-overlay
                     (not (seq-some (lambda (ov)
                                      (eq (overlay-get ov 'category) 'slacko-emoji))
                                    (overlays-at mb))))
            (slacko-emoji--make-overlay mb me unicode shortcode)
            ;; superscript the count if present - overlay covers space+digits
            (when (match-beginning 2)
              (slacko-emoji--make-count-overlay
               (match-beginning 2) (match-end 2)
               (match-string 3)))))))))

(defun slacko-emoji--emojify-buffer ()
  "Place emoji overlays on the entire current buffer."
  (slacko-emoji--emojify-region (point-min) (point-max)))

(defun slacko-emoji--remove-overlays ()
  "Remove all slacko-emoji overlays from the current buffer."
  (setq slacko-emoji--revealed-overlays nil)
  (remove-overlays (point-min) (point-max) 'category 'slacko-emoji))

;;; Reveal-at-point (org-appear style)

(defun slacko-emoji--overlay-at-point ()
  "Return the slacko-emoji overlay at point, or nil."
  (seq-find (lambda (ov) (eq (overlay-get ov 'category) 'slacko-emoji))
            (overlays-at (point))))

(defun slacko-emoji--reveal (ov)
  "Reveal the shortcode text for overlay OV by removing its display property.
Only reveals emoji overlays, not count overlays."
  (when (and ov
             (overlay-get ov 'display)
             (not (overlay-get ov 'slacko-emoji-count)))
    (overlay-put ov 'display nil)
    (push ov slacko-emoji--revealed-overlays)))

(defun slacko-emoji--conceal (ov)
  "Re-conceal overlay OV, restoring its emoji display."
  (when ov
    (when-let* ((shortcode (overlay-get ov 'slacko-emoji-shortcode))
                (unicode (slacko-emoji--shortcode-to-unicode shortcode)))
      (overlay-put ov 'display unicode))
    (setq slacko-emoji--revealed-overlays
          (delq ov slacko-emoji--revealed-overlays))))

(defun slacko-emoji--post-command ()
  "Reveal emoji at point; conceal previously revealed ones elsewhere."
  (let ((current-ov (slacko-emoji--overlay-at-point)))
    ;; conceal any previously revealed overlays that aren't at point
    (dolist (ov slacko-emoji--revealed-overlays)
      (unless (eq ov current-ov)
        (slacko-emoji--conceal ov)))
    ;; reveal the one at point
    (when current-ov
      (slacko-emoji--reveal current-ov))))

;;; After-change hook for dynamic content

(defun slacko-emoji--after-change (beg end _len)
  "Re-emojify the changed region from BEG to END."
  ;; expand to line boundaries for safety
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
        (line-end (save-excursion (goto-char end) (line-end-position))))
    ;; remove existing emoji overlays in the region first
    (dolist (ov (overlays-in line-beg line-end))
      (when (eq (overlay-get ov 'category) 'slacko-emoji)
        (delete-overlay ov)))
    (slacko-emoji--emojify-region line-beg line-end)))

;;; Interactive commands

(defun slacko-emoji-show-text-at-point ()
  "Show the original shortcode for the emoji at point."
  (interactive)
  (if-let* ((ov (slacko-emoji--overlay-at-point))
            (shortcode (overlay-get ov 'slacko-emoji-shortcode)))
      (message "%s" shortcode)
    (message "No emoji at point")))

;;; Minor mode

;;;###autoload
(define-minor-mode slacko-emoji-mode
  "Toggle emoji overlay display in the current buffer.
When enabled, Slack shortcodes like :smile: are overlaid with their
Unicode emoji equivalents.  The original text is revealed when the
cursor is on an emoji (org-appear style)."
  :lighter " 😀"
  :group 'slacko-emoji
  (if slacko-emoji-mode
      (progn
        ;; ensure emojify data is loaded
        (unless (and (boundp 'emojify-emojis)
                     (hash-table-p emojify-emojis)
                     (> (hash-table-count emojify-emojis) 0))
          (emojify-create-emojify-emojis))
        (slacko-emoji--emojify-buffer)
        (add-hook 'post-command-hook #'slacko-emoji--post-command nil t)
        (add-hook 'after-change-functions #'slacko-emoji--after-change nil t))
    (slacko-emoji--remove-overlays)
    (remove-hook 'post-command-hook #'slacko-emoji--post-command t)
    (remove-hook 'after-change-functions #'slacko-emoji--after-change t)))

;;; Auto-enable in GUI

(defun slacko-emoji--maybe-enable ()
  "Enable `slacko-emoji-mode' if running in a graphical display."
  (when (display-graphic-p)
    (slacko-emoji-mode 1)))

(with-eval-after-load 'slacko
  (add-hook 'slacko-search-mode-hook #'slacko-emoji--maybe-enable))

(with-eval-after-load 'slacko-thread
  (add-hook 'slacko-thread-mode-hook #'slacko-emoji--maybe-enable))

(provide 'slacko-emoji)
;;; slacko-emoji.el ends here
