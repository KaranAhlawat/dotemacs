;;; init.el --- Initial file read by Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; This file ties all the modules together
;;; Code:

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(set-default-coding-systems 'utf-8)

;; Straight.el is my current package manager of choice, even with the
;; recent improvements to package.el in Emacs >=29. Also, since
;; use-package is now built into Emacs itself, I'll be opting to use
;; that and it's straight.el intergration.

;; Here we're just bootstraping straight.el as noted in their github README.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setting the custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)

(require 'ui)
(require 'completions)
(require 'general)
(require 'development)
(require 'languages)
(require 'shells)

(setq gc-cons-threshold (* 2 1000 1000))
;; init.el ends here
