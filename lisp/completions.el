;; completion.el --- Completions for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This packages sets up various completion providers
;;; Code:

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

;; Those quick annotations are really helpful
(use-package marginalia
  :custom
  (marginalika-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*.el")
                   :includes (vertico-directory))
  :demand t
  :bind
  (:map vertico-map
        ("TAB" . #'vertico-insert)
        ("RET" . #'vertico-exit))
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-preselect 'directory)
  :config (vertico-mode))

(use-package minibuffer
  :ensure nil
  :bind ( :map minibuffer-local-completion-map
          ("<up>" . minibuffer-previous-line-completion)
          ("<down>" . minibuffer-next-line-completion))
  :init
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select 'second-tab)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t)
  ;; (setq minibuffer-visible-completions t) ; Emacs 30
  (setq completions-sort nil)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq enable-recursive-minibuffers t)
  :config
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; A few more useful configurations
(use-package crm
  :ensure nil
  :config
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' with ARGS.
We display [CRM<separator>], e.g., [CRM,] if the separator is a
comma."
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Tempel for expansion - May remove this in the future as I don't use it alot.
(use-package tempel
  :bind (("M-+" . #'tempel-complete)
         ("M-*" . #'tempel-insert)
         :map tempel-map
         ("TAB" . #'tempel-next))
  :config
  (setq tempel-path (locate-user-emacs-file "templates/*.eld")))

(use-package orderless
  :after vertico
  :custom-face
  (orderless-match-face-0 ((t (:weight ,conf/weight))))
  (orderless-match-face-1 ((t (:weight ,conf/weight))))
  (orderless-match-face-2 ((t (:weight ,conf/weight))))
  (orderless-match-face-3 ((t (:weight ,conf/weight))))
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil
        orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex)))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package company
  :ensure t
  :demand t
  :bind ( :map company-active-map
          ("<tab>" . company-select-next)
          ("<backtab>" . company-select-previous)
          ("RET" . company-complete-selection))
  ;; :hook (elpaca-after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.3)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-flip-when-above nil)
  (company-format-margin-function #'company-text-icons-margin)
  (company-text-icons-add-background t)
  (company-text-face-extra-attributes '(:weight bold))
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-preview-frontend))
  (company-backends '(company-capf
                      company-files
                      company-dabbrev))
  (company-transformers '(company-sort-by-occurrence
                          company-sort-prefer-same-case-prefix))
  :custom-face
  (company-tooltip-common ((t (:weight bold))))
  (company-tooltip-search ((t (:weight ,conf/weight))))
  (company-tooltip-selection ((t (:weight ,conf/weight))))
  :config
  (keymap-global-set "TAB" #'company-indent-or-complete-common))

(use-package consult
  :demand t
  :bind (("M-g i" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-c r" . consult-recent-file)
         ("M-y" . consult-yank-pop))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq
   consult--tofu-char #x100000
   consult--tofu-range #xFFFE)

  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   consult-project-buffer
   consult-recent-file
   :preview-key "M-."))

(use-package consult-flycheck
  :after consult
  :bind ("M-g f" . consult-flycheck))

(use-package consult-lsp
  :after consult
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lsp-treemacs-errors-lisp] #'consult-lsp-diagnostics))

(provide 'completions)
;;; completions.el ends here
