;;; init.el --- Initial file read by Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; This file ties all the modules together
;;; Code:
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
(eval-and-compile
	(add-to-list
	 'load-path
	 (concat (file-name-as-directory user-emacs-directory) "lisp/")))

(set-default-coding-systems 'utf-8)
(setq load-suffixes (list ".so" ".eln" ".elc" ".el"))
(setq load-prefer-newer t)

;; Setting the custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when (file-exists-p custom-file)
	            (load custom-file nil 'nomessage))))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)

(require 'setup-org)
(require 'windows)

(use-package popper
  :demand t
	:bind
	(("C-`" . popper-toggle)
	 ("C-M-`" . popper-cycle)
	 ("C-c p t" . popper-toggle-type)
	 ("C-M-q" . popper-kill-latest-popup))
	:init
	(setq popper-reference-buffers
				(append
				 conf/help-modes-list
				 conf/man-modes-list
				 conf/repl-modes-list
				 conf/repl-names-list
				 conf/occur-grep-modes-list
				 '(Custom-mode (compilation-mode . hide) messages-buffer-mode)
				 '(("^\\*Warnings\\*$" . hide)
					 ("^\\*Compile-Log\\*$" . hide)
					 "^\\*Backtrace\\*"
					 "^\\*Apropos"
					 "^Calc:"
					 "^\\*eldoc\\(.*\\)\\*"
					 "^\\*TeX errors\\*"
					 "^\\*ielm\\*"
					 "^\\*TeX Help\\*"
					 "\\*Shell Command Output\\*"
					 ("\\*Async Shell Command\\*" . hide)
					 "\\*Completions\\*"
					 "[Oo]utput\\*"
					 "\\*EGLOT\\(.*\\)\\*"
           "^\\*tree-sitter explorer for \\(.+\\)\\*"
					 ("^\\*straight-process\\*" . hide)
					 ("^\\*straight-byte-compilation\\*" . hide)
					 ("^\\*Async-native-compile-log" . hide))))

	(setq popper-display-function
				(defun conf/popper-select-below (buffer &optional _alist)
					(funcall (if (> (frame-width) 170)
											 #'popper-select-popup-at-bottom
										 #'display-buffer-at-bottom)
									 buffer
									 `((window-height . ,popper-window-height)
										 (direction . below)
										 (body-function . ,#'select-window)))))
  (setq popper-display-control t)

  :config
	(advice-add
	 'popper-cycle
	 :after
	 (defun conf/popper-cycle-repeated (&rest _)
		 "Continue to cycle popups with the grave key."
		 (set-transient-map
			(let ((map (make-sparse-keymap)))
				(define-key map (kbd "`") #'popper-cycle)
				map))))

	(use-package popper-echo
    :ensure nil
	  :init
	  (defun popper-message-shorten (name)
		  (cond
			 ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
			  (concat (match-string 1 name) "(H)"))
			 ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
			  (concat (match-string 1 name) "(H)"))
			 ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
			  (concat
				 (match-string 1 name)
				 (if (string-empty-p (match-string 1 name))
						 "shell(E)"
					 "(E)")))
			 ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
			  (concat (match-string 1 name) "(O)"))
			 ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
			  (concat (match-string 1 name) "(L)"))
			 ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
			  (concat (match-string 1 name) "(C)"))
			 (t
			  name)))
	  (setq popper-mode-line
				  '(:eval (propertize " POP " 'face 'mode-line-highlight)))
	  (setq popper-echo-transform-function #'popper-message-shorten)
	  (setq
		 popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
		 popper-echo-dispatch-actions t)
    :config
	  (advice-add
		 'popper-echo
		 :around
		 (defun conf/popper-echo-no-which-key (orig-fn)
			 (let ((which-key-show-transient-maps nil))
				 (funcall orig-fn))))
	  (popper-echo-mode))

	(popper-mode))

(use-package custom
  :ensure nil
	:init
	(defvar after-enable-theme-hook nil)
	(defun run-after-enable-theme-hook (&rest _args)
		(run-hooks 'after-enable-theme-hook))
	:config
	(declare-function run-after-enable-theme-hook "init")
	(advice-add 'enable-theme :after #'run-after-enable-theme-hook))

(require 'general)
(require 'keybindings)
(require 'ui)
(require 'ligate)
(require 'defaults)
(require 'completions)
(require 'languages)
(require 'development)
(require 'shells)
(require 'setup-latex)
(require 'reader)

(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
