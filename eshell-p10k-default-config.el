;;; eshell-p10k-default-config.el --- Default segments for eshell-p10k -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: February 19, 2021
;; Modified: February 19, 2021
;; Version: 0.0.2
;; Homepage: https://github.com/elken/eshell-p10k
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Default segment implementations for eshell-p10k.
;; Users can define custom segments using `eshell-p10k-def-segment'.
;;
;;; Code:
(require 'eshell-p10k)
(require 'nerd-icons)
(require 'cl-lib)
(autoload 'vc-find-root "vc-hooks")

;;; Faces

(defface eshell-p10k-directory-face
  '((t (:background "steel blue")))
  "Face for the current working directory."
  :group 'eshell-p10k-faces)

(defface eshell-p10k-prompt-face
  '((t (:background "brown")))
  "Face for the prompt number."
  :group 'eshell-p10k-faces)

(defface eshell-p10k-distro-face
  '((t (:background "white" :foreground "black")))
  "Face for the OS/distro icon."
  :group 'eshell-p10k-faces)

(defface eshell-p10k-git-clean-face
  '((t :background "forest green"))
  "Face for a clean git repository."
  :group 'eshell-p10k-faces)

(defface eshell-p10k-git-dirty-face
  '((t :background "indian red"))
  "Face for a dirty git repository."
  :group 'eshell-p10k-faces)

;;; OS/Distro segment

(defconst eshell-p10k--distro-icons
  '(("arch"       . "nf-md-arch")
    ("debian"     . "nf-md-debian")
    ("raspbian"   . "nf-md-raspberry_pi")
    ("ubuntu"     . "nf-md-ubuntu")
    ("fedora"     . "nf-md-fedora")
    ("gentoo"     . "nf-md-gentoo")
    ("centos"     . "nf-md-centos")
    ("linuxmint"  . "nf-md-linux_mint")
    ("nixos"      . "nf-md-nix"))
  "Mapping of distro names to nerd-icon names.")

(defun eshell-p10k--distro-id ()
  "Get Linux distribution ID from /etc/os-release."
  (when (file-exists-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (goto-char (point-min))
      (when (re-search-forward "^ID=\\(.+\\)$" nil t)
        (string-trim (match-string 1) "\"" "\"")))))

(defun eshell-p10k--linux-icon ()
  "Get icon for current Linux distribution."
  (let* ((distro (eshell-p10k--distro-id))
         (icon-name (or (alist-get distro eshell-p10k--distro-icons nil nil #'string=)
                       "nf-md-linux")))
    (propertize (nerd-icons-mdicon icon-name)
                'face '(:height 1)
                'display '(raise 0.0))))

(defun eshell-p10k--os-icon ()
  "Get icon for current operating system."
  (pcase system-type
    ((or 'cygwin 'windows-nt 'ms-dos)
     (nerd-icons-mdicon "nf-md-microsoft_windows"))
    ('darwin
     (propertize (nerd-icons-mdicon "nf-md-apple")
                 'face '(:height 1)
                 'display '(raise 0.0)))
    ('berkeley-unix
     (nerd-icons-mdicon "nf-md-freebsd"))
    ('gnu/linux
     (eshell-p10k--linux-icon))))

(eshell-p10k-def-segment distro
  nil
  (eshell-p10k--os-icon)
  'eshell-p10k-distro-face)

;;; Directory segment

(eshell-p10k-def-segment dir
  ""
  (abbreviate-file-name (eshell/pwd))
  'eshell-p10k-directory-face)

;;; Git segment

(defun eshell-p10k--git-repo-p ()
  "Return non-nil if in a git repository."
  (and (executable-find "git")
       (vc-find-root (eshell/pwd) ".git")))

(defun eshell-p10k--git-command (command)
  "Run git COMMAND and return trimmed output."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'process-file "git" nil t nil (split-string command))))))

(defun eshell-p10k--git-branch ()
  "Get current git branch name."
  (when (eshell-p10k--git-repo-p)
    (eshell-p10k--git-command "symbolic-ref --short HEAD")))

(defun eshell-p10k--git-clean-p ()
  "Return non-nil if git repository is clean."
  (and (eshell-p10k--git-branch)
       (string-empty-p (eshell-p10k--git-command "status -s"))))

(defun eshell-p10k--git-status-counts ()
  "Return alist of git status counts."
  (when (eshell-p10k--git-repo-p)
    (let* ((output (eshell-p10k--git-command "status --porcelain"))
           (lines (and (not (string-empty-p output))
                      (split-string output "\n" t)))
           (counts nil))
      (dolist (line lines)
        (let* ((status (substring line 0 2))
               (pair (assoc status counts)))
          (if pair
              (setcdr pair (1+ (cdr pair)))
            (push (cons status 1) counts))))
      counts)))

(defun eshell-p10k--git-status ()
  "Format git status for display."
  (when-let* ((branch (eshell-p10k--git-branch))
              (counts (eshell-p10k--git-status-counts))
              (status-str (mapconcat (lambda (c) (format "%s%d" (car c) (cdr c)))
                                    counts " ")))
    (concat branch " " status-str)))

(eshell-p10k-def-segment git
  "󰊢"
  (or (eshell-p10k--git-status)
      (eshell-p10k--git-branch))
  (if (eshell-p10k--git-clean-p)
      'eshell-p10k-git-clean-face
    'eshell-p10k-git-dirty-face))

;;; Prompt number segment

(defvar eshell-p10k--prompt-num 0
  "Current prompt number.")

(defun eshell-p10k--increment-prompt-num ()
  "Increment the prompt counter."
  (cl-incf eshell-p10k--prompt-num))

(defun eshell-p10k--reset-prompt-num ()
  "Reset the prompt counter."
  (setq eshell-p10k--prompt-num 0))

(add-hook 'eshell-exit-hook #'eshell-p10k--reset-prompt-num)
(advice-add 'eshell-send-input :before #'eshell-p10k--increment-prompt-num)
(advice-add 'eshell-interrupt-process :before #'eshell-p10k--increment-prompt-num)

(eshell-p10k-def-segment prompt-num
  "󰍜"
  (number-to-string eshell-p10k--prompt-num)
  'eshell-p10k-prompt-face)

;;; Default prompt

;;;###autoload
(defun eshell-p10k-default-prompt ()
  "Default eshell-p10k prompt configuration."
  (eshell-p10k-def-prompt '(distro dir git prompt-num)))

(provide 'eshell-p10k-default-config)
;;; eshell-p10k-default-config.el ends here
