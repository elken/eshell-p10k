;;; eshell-p10k.el --- Custom prompt framework for eshell -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: February 19, 2021
;; Modified: February 19, 2021
;; Version: 0.0.2
;; Homepage: https://github.com/elken/eshell-p10k
;; Package-Requires: ((emacs "27.1") (nerd-icons "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Fancy prompt framework based on p10k for eshell.
;; Provides tools to build custom segments and compose prompts.
;;
;; Basic usage:
;;
;;   (require 'eshell-p10k-default-config)
;;   (setq eshell-prompt-function #'eshell-p10k-default-prompt)
;;
;; Define custom segments:
;;
;;   (eshell-p10k-def-segment time
;;     ""
;;     (format-time-string "%H:%M:%S")
;;     'my-time-face)
;;
;; Create custom prompts:
;;
;;   (defun my-prompt ()
;;     (eshell-p10k-def-prompt '(distro dir git time)))
;;
;;; Code:
(require 'em-dirs)
(require 'cl-lib)

;;; Customization

(defgroup eshell-p10k nil
  "Settings related to eshell-p10k."
  :group 'eshell)

(defgroup eshell-p10k-faces nil
  "Faces related to eshell-p10k."
  :group 'eshell-p10k)

(defcustom eshell-p10k-header-string "\n┌─"
  "String to append before the first segment."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-prompt-string "└─> "
  "String to append just before the user's input."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-prompt-regex "^.*─> "
  "Regex to find the prompt tip (used for jumping between prompts)."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-separator "\xe0bc"
  "String to insert between segments."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-start-terminator "\xe0b2"
  "String to insert at the start of segments."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-end-terminator "\xe0b0"
  "String to insert at the end of segments."
  :type 'string
  :group 'eshell-p10k)

;;; Core framework

(defvar eshell-p10k-segment-alist ()
  "Alist mapping segment names to their rendering functions.")

(defun eshell-p10k--segment-name (segment)
  "Convert SEGMENT symbol to internal name."
  (intern (format "eshell-p10k-segment--%s" segment)))

(defmacro eshell-p10k-def-segment (name icon form face)
  "Define a prompt segment.
NAME is the segment identifier symbol.
ICON is a string to display before the content (or nil).
FORM is evaluated to get the segment content.
FACE is the face to style the segment."
  (declare (indent defun))
  `(setq eshell-p10k-segment-alist
         (cons (cons ',(eshell-p10k--segment-name name)
                     (lambda ()
                       (when-let ((content ,form))
                         (propertize
                          (concat ,@(when icon `((concat ,icon " "))) content " ")
                          'face ,face))))
               (assq-delete-all ',(eshell-p10k--segment-name name)
                               eshell-p10k-segment-alist))))

(defun eshell-p10k--get-bg (face)
  "Get background color from FACE."
  (face-background face))

(defun eshell-p10k--face-at (string pos)
  "Get face at POS in STRING."
  (get-text-property pos 'face string))

(defun eshell-p10k--separator (left-section right-section)
  "Create separator between LEFT-SECTION and RIGHT-SECTION."
  (let ((left-bg (eshell-p10k--get-bg (eshell-p10k--face-at left-section (- (length left-section) 2))))
        (right-bg (eshell-p10k--get-bg (eshell-p10k--face-at right-section 0))))
    (propertize (concat eshell-p10k-separator " ")
                'face `(:foreground ,left-bg :background ,right-bg))))

(defun eshell-p10k--start-terminator (section)
  "Create start terminator for SECTION."
  (propertize eshell-p10k-start-terminator
              'face `(:foreground ,(eshell-p10k--get-bg (eshell-p10k--face-at section 0)))))

(defun eshell-p10k--end-terminator (section)
  "Create end terminator for SECTION."
  (propertize eshell-p10k-end-terminator
              'face `(:foreground ,(eshell-p10k--get-bg (eshell-p10k--face-at section (- (length section) 2))))))

(defun eshell-p10k--render-segments (segments)
  "Render SEGMENTS list into a prompt string."
  (let ((result ""))
    (dolist (segment-name segments)
      (when-let* ((segment-fn (alist-get (eshell-p10k--segment-name segment-name)
                                         eshell-p10k-segment-alist))
                  (rendered (funcall segment-fn)))
        (setq result
              (if (string-empty-p result)
                  (concat (eshell-p10k--start-terminator rendered) rendered)
                (concat result (eshell-p10k--separator result rendered) rendered)))))
    result))

(defun eshell-p10k-def-prompt (segments)
  "Build a prompt from SEGMENTS list.
SEGMENTS is a list of segment name symbols."
  (let ((prompt (eshell-p10k--render-segments segments)))
    (concat eshell-p10k-header-string
            prompt
            (eshell-p10k--end-terminator prompt)
            "\n"
            eshell-p10k-prompt-string)))

(provide 'eshell-p10k)
;;; eshell-p10k.el ends here
