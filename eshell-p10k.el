;;; eshell-p10k.el --- p10k-style prompt for eshell -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: February 19, 2021
;; Modified: February 19, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/elken/eshell-p10k
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Fancy prompt based on p10k made purely in eshell.
;; Also includes tools to build custom segments
;;
;;; Code:
(require 'em-dirs)
(require 'cl-lib)
(require 'dash)
(require 's)
(autoload 'vc-find-root "vc-hooks")
(autoload 'vc-git-branches "vc-git")

;; Groups

(defgroup eshell-p10k nil
  "Settings related to eshell-p10k"
  :group 'eshell-p10k)

(defgroup eshell-p10k-faces nil
  "Faces related to eshell-p10k"
  :group 'eshell-p10k-faces)

;; Faces

(defface eshell-exit-success-face
  '((t (:inherit success)))
  "Face to indicate the previous command exited successfully"
  :group 'eshell-p10k-faces)

(defface eshell-exit-failure-face
  '((t (:inherit fail)))
  "Face to indicate the previous command exited unsuccessfully"
  :group 'eshell-p10k-faces)

(defface eshell-directory-face
  '((t (:background "steel blue")))
  "Face for the current working directory"
  :group 'eshell-p10k-faces)

(defface eshell-prompt-face
  '((t (:background "brown")))
  "Face for the prompt number"
  :group 'eshell-p10k-faces)

(defface eshell-distro-face
  '((t (:background "white" :foreground "black")))
  "Face for the distro icon"
  :group 'eshell-p10k-faces)

(defface eshell-git-modified-face
  '((t (:foreground "red")))
  "Face for the 'modified' item in git"
  :group 'eshell-p10k-faces)

(defface eshell-git-add-face
  '((t (:foreground "white")))
  "Face for the 'added' item in git"
  :group 'eshell-p10k-faces)

(defface eshell-git-branch-face
  '((t (:foreground "dark gray")))
  "Face for the current git branch"
  :group 'eshell-p10k-faces)

(defface eshell-git-clean-face
  '((t :background "forest green"))
  "Face for a clean git tree"
  :group 'eshell-p10k-faces)

(defface eshell-git-dirty-face
  '((t :background "indian red"))
  "Face for a dirty git tree"
  :group 'eshell-p10k-faces)

;; Custom variables

(defcustom eshell-p10k-header-string "\n┌─"
  "String to append before the first segment."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-prompt-string "└─> "
  "String to append just before the user's input."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-prompt-regex "^.*─> "
  "Regex to use to find the tip of the prompt (used for jumping between prompts)."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-separator "\xe0bc"
  "String to insert between segments."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-start-terminator "\xe0b2"
  "String to insert at the start of the segments."
  :type 'string
  :group 'eshell-p10k)

(defcustom eshell-p10k-end-terminator "\xe0b0"
  "String to insert at the end of the segments."
  :type 'string
  :group 'eshell-p10k)

;; Core

(defvar eshell-p10k-segment-alist ())

(defmacro eshell-p10k--internal-segment-name (segment)
  "Convert SEGMENT to the internal name."
  `(intern (format "eshell-p10k-segment--%s" ,segment)))

(defmacro eshell-p10k-def-segment (NAME ICON FORM FACE)
  "Define a segment called NAME with ICON, evaluating FORM and styling with FACE."
  (declare (indent defun))
  (let ((name (eshell-p10k--internal-segment-name NAME)))
    `(add-to-list 'eshell-p10k-segment-alist (cons ',name
                                                   (lambda () (when ,FORM
                                                           (-> " "
                                                               (concat (when ,ICON (concat ,ICON " ")) ,FORM " ")
                                                               (propertize 'face ,FACE))))))))

(defun eshell-p10k--get-section-separator (acc section)
  "Return an appropriately-coloured CHAR separator.
Needs SECTION for current background and ACC for existing
foreground"
  (let ((foreground (get-text-property (- (length acc) 2) 'face acc))
        (background (get-text-property 0 'face section)))
    (propertize (format "%s " eshell-p10k-separator) 'face `(:foreground ,(face-background foreground) :background ,(face-background background)))))

(defun eshell-p10k--get-prompt-start-terminator (section)
  "Return an appropraitely-coloured CHAR terminator.
Needs SECTION for current foreground"
  (propertize eshell-p10k-start-terminator 'face `(:foreground ,(face-background (get-text-property 0 'face section)))))

(defun eshell-p10k--get-prompt-end-terminator (section)
  "Return an appropraitely-coloured CHAR terminator.
Needs SECTION for current foreground"
  (propertize eshell-p10k-end-terminator 'face `(:foreground ,(face-background (get-text-property (- (length section) 2) 'face section)))))

(defun eshell-p10k--accumulator (acc x)
  "Given previous result ACC, append reuslt of calling X."
  (--if-let (funcall x)
      (let ((header (eshell-p10k--get-prompt-start-terminator it)))
        (if (s-blank? acc)
            (concat header it)
          (concat acc (eshell-p10k--get-section-separator acc it) it)))
    acc))

(defun eshell-p10k--prepare-segments (segments)
  "Ensure the SEGMENTS are valid and exist."
  (let (forms it)
    (dolist (segment segments)
      (cond ((stringp segment)
             (push segment forms))
            ((symbolp segment)
             (setq it (cdr (assq (eshell-p10k--internal-segment-name segment) eshell-p10k-segment-alist)))
             (push it forms))
            ((error "%s is not a defined eshell segment" segment))))
    (nreverse forms)))

(defun eshell-p10k-def-prompt (segments)
  "Given a list of SEGMENTS, evaluate in order and return."
  (let* ((segment-forms (eshell-p10k--prepare-segments segments))
         (prompt (-reduce-from 'eshell-p10k--accumulator "" segment-forms)))
    (concat eshell-p10k-header-string
            prompt
            (eshell-p10k--get-prompt-end-terminator prompt)
            "\n"
            eshell-p10k-prompt-string)))

(defun eshell-p10k--last-command-success-p ()
  "Return t if the last command was successful, else nil."
  (= (if (not (boundp 'eshell-last-command-status))
         0
       eshell-last-command-status) 0))

(defun eshell-p10k--get-linux-icon ()
  "Attempt to get an icon for the current linux distro."
  (propertize
   (if (not (file-exists-p "/etc/os-release"))
       (all-the-icons-faicon "linux")
     (let ((distro (string-trim (shell-command-to-string "rg '^ID=' /etc/os-release | cut -d= -f2"))))
       (pcase distro
         ("arch" "\uF303")
         ("debian" "\uF306")
         ("raspbian" "\uF315")
         ("ubuntu" "\uF31b")
         ("elementary" "\uF309")
         ("fedora" "\uF30a")
         ("coreos" "\uF305")
         ("gentoo" "\uF30d")
         ("mageia" "\uF310")
         ("centos" "\uF304")
         ((or "opensuse" "tumbleweed") "\uF314")
         ("sabayon" "\uF317")
         ("slackware" "\uF319")
         ("linuxmint" "\uF30e")
         ("alpine" "\uF300")
         ("aosc" "\uF301")
         ("nixos" "\uF313")
         ("devuan" "\uF307")
         ("manjaro" "\uF312")
         ((or "void" "artix") "\uF17c")
         (_ (all-the-icons-faicon "linux")))))
     'face '(:height 1)
     'display '(raise 0.0)))

(defun eshell-p10k--get-os-icon ()
  "Return an icon based on the current operating system."
  (cond
   ((memq system-type '(cygwin windows-nt ms-dos)) (all-the-icons-faicon "windows"))
   ((eq system-type 'darwin)                       (all-the-icons-fileicon "apple"))
   ((eq system-type 'berkely-unix)                 "\uF30c ")
   ((eq system-type 'gnu/linux)                    (eshell-p10k--get-linux-icon))))

(defun eshell-p10k-prompt-function ()
  "Prompt defining function."
  (eshell-p10k-def-prompt '(distro dir git prompt-num)))

(eshell-p10k-def-segment distro
  nil
  (eshell-p10k--get-os-icon)
  'eshell-distro-face)

(eshell-p10k-def-segment dir
  ""
  (abbreviate-file-name (eshell/pwd))
  'eshell-directory-face)

(defun eshell-p10k-git--repo-p ()
  "Return t if the git executable is on the system and we're in a git repo."
  (and (executable-find "git")
       (vc-find-root (eshell/pwd) ".git")))

(defun eshell-p10k-git--command (command &optional directory)
  "Run a git COMMAND in the current directory, or DIRECTORY."
  (let ((default-directory (or directory default-directory))
        (process-environment process-environment))
    (let ((result (s-trim (with-output-to-string
                            (apply #'call-process
                                   (executable-find "git")
                                   nil
                                   standard-output
                                   nil
                                   (s-split " " command))))))
      (when (and init-file-debug
                 (s-starts-with-p "fatal:" result))
        (message "git error: %s %s" command result))
      result)))

(defun eshell-p10k-git-current-branch ()
  "Return the current git branch."
  (when (eshell-p10k-git--repo-p)
    (eshell-p10k-git--command "symbolic-ref --short HEAD")))

(defun eshell-p10k-git--clean-p ()
  "Predicate to check if the current directory is clean."
  (when (eshell-p10k-git-current-branch)
    (string= "" (eshell-p10k-git--command "status -s"))))

(defun alist-set-or-increment (lst key &optional symbol)
  "Either set KEY to VAL in LST, or increment if exists."
  (if-let ((pair (if symbol
                     (assq key lst)
                   (assoc key lst))))
      (setcdr pair (+ 1 (cdr pair)))
    (push (cons key 1) lst))
  lst)

(defun eshell-p10k-git--status ()
  "Return an alist based on the current git status."
  (when (eshell-p10k-git--repo-p)
    (when-let* ((status-lines (s-split "\n" (eshell-p10k-git--command "status --porcelain"))))
      (mapconcat 'identity (mapcar
                            (lambda (item) (format "%s%s" (car item) (cdr item)))
                            (-reduce-from
                             (lambda (acc x)
                               (alist-set-or-increment acc (substring x 0 2)))
                             '()
                             status-lines)) " "))))

(defun eshell-p10k-git-status ()
  "Return the current git status."
  (when (eshell-p10k-git--repo-p)
    (concat
     (eshell-p10k-git-current-branch)
     " "
     (eshell-p10k-git--status))))

(eshell-p10k-def-segment git
  ""
  (eshell-p10k-git-status)
  (if (eshell-p10k-git--clean-p)
      'eshell-git-clean-face
    'eshell-git-dirty-face))

(defvar eshell-p10k--prompt-num-index 0)
(add-hook 'eshell-exit-hook (lambda () (setq eshell-p10k--prompt-num-index 0)))

(defun eshell-p10k-prompt-num-increment ()
  "Increment the current prompt index."
  (interactive)
  (setq eshell-p10k--prompt-num-index (cl-incf eshell-p10k--prompt-num-index)))

(advice-add 'eshell-send-input :before 'eshell-p10k-prompt-num-increment)
(advice-add 'eshell-interrupt-process :before 'eshell-p10k-prompt-num-increment)

(eshell-p10k-def-segment prompt-num
  ""
  (number-to-string eshell-p10k--prompt-num-index)
  'eshell-prompt-face)

(provide 'eshell-p10k)
;;; eshell-p10k.el ends here
