;;; general.el --- General packages and configurations -*- lexical-binding: t; -*-
;;; Commentary:
;;; General stuff that I don't think belongs to other modules but is
;;; too small to create it's own module
;;; Code:

(use-package emacs
  :ensure nil
  :init
  (setq frame-title-format "%b")
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (setq visible-bell nil)
  (setq global-auto-revert-ignore-modes '(Buffer-menu-mode))
  (setq backup-by-copying t)
  (setq use-dialog-box nil)
  (setq xref-search-program 'ripgrep)
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  :config
  (blink-cursor-mode -1))

;; Makes it easier to see where what is
(use-package rainbow-delimiters
  :hook
  (lisp-mode emacs-lisp-mode cider-mode cider-repl-mode clojure-mode))

;; Either act on the whole line or the current region
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

(use-package smtpmail
  :ensure nil
  :config
  (setq
   user-full-name "Karan Ahlawat"
   user-mail-address "ahlawatkaran12@gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-serice 587
   send-mail-function 'smtpmail-send-it))

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '(".project"
     "project.clj"
     "package.json"
     "deno.json"
     "build.sbt"
     "build.sc"
     "mix.exs"
     "Cargo.toml"
     "*.fsproj"
     "*.sln"
     "dune-project"))
  (project-vc-ignores
   '("node_modules"
     "target"
     "out"
     "_build"
     ".gradle"
     "build"
     "_opam"
     "esy.lock"
     "_esy"
     "deps"
     ".lexical"
     ".elixir_ls"
     "dist"
     "_build"
     "straight"
     "var"
     "elpa"
     "etc"
     "tree-sitter"
     "lsp-cache"
     "eln-cache"
     ".git"))
  :config
	(setq popper-group-function #'popper-group-by-project)
  (setopt
   project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (project-eshell "Eshell")))

  (keymap-set project-prefix-map "." #'project-dired)
  (keymap-set project-prefix-map "<return>" #'project-dired)
  (keymap-set project-prefix-map "<delete>" #'project-forget-project))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode) (dired-mode . hl-line-mode))
  :config
  (setq
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   delete-by-moving-to-trash t
   dired-listing-switches "-aGFhlv --group-directories-first --time-style=long-iso"
   dired-dwim-target t
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-make-directory-clickable t
   dired-free-space nil
   dired-mouse-drag-files t))

(use-package dired-aux
  :ensure nil
  :config
  (setq
   dired-isearch-filenames 'dwim
   dired-create-destination-dirs 'ask
   dired-vc-rename-file t
   dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))))

(use-package dired-x
  :ensure nil
  :config
  (setq
   dired-clean-up-buffers-too t
   dired-clean-confirm-killing-deleted-buffers t
   dired-x-hands-off-my-keys t))

(use-package pulsar
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  :config
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (pulsar-global-mode +1))

(repeat-mode +1)

(use-package ctrlf
  :init
  (setq ctrlf-default-search-style 'fuzzy)
  :config
  (ctrlf-mode +1))

(use-package nerd-icons
  :custom
  (nerd-icons-scale-factor 1.2))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 50)
  (add-hook 'elpaca-after-init-hook (lambda ()
                               (recentf-load-list)
                               (recentf-mode))))

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h x" . #'helpful-command)
         ("C-h F" . #'helpful-function)
         :map emacs-lisp-mode-map
         ("C-c C-d" . #'helpful-at-point)))

(provide 'general)
;;; general.el ends here
