;;; init.el --- Initial file read by Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; This file ties all the modules together
;;; Code:
(setq-default lexical-binding t
              load-prefer-newer t)

(defvar conf--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Add ~/.config/emacs/lisp to the load-path
(add-to-list
 'load-path
 (concat (file-name-as-directory user-emacs-directory) "lisp/"))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(elpaca-wait)

(setq use-package-always-ensure t
      use-package-verbose nil
      use-package-expand-minimally byte-compile-current-file)

(use-package custom
  :ensure nil
  :init
  (defvar after-enable-theme-hook nil)
  (defun run-after-enable-theme-hook (&rest _args)
    (run-hooks 'after-enable-theme-hook))
  :config
  (declare-function run-after-enable-theme-hook "init")
  (advice-add 'enable-theme :after #'run-after-enable-theme-hook))

(require 'ui)
(require 'setup-org)
(require 'windows)
(require 'general)
(require 'keybindings)
(require 'ligate)
(require 'defaults)
(require 'completions)
(require 'languages)
(require 'development)
(require 'shells)
(require 'setup-latex)
(require 'reader)

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)
(setq file-name-handler-alist conf--file-name-handler-alist)

(provide 'init)
;;; init.el ends here
