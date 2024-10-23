;;; development.el --- Making the money -*- lexical-binding: t; -*-
;;; Commentary:
;;; Sets up a great dev exp inside Emacs
;;; Code:

;; Magit, the magical git interface
(use-package transient)
(use-package magit
  :after transient
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; ((heex-ts-mode
;;            elixir-ts-mode
;;            java-ts-mode
;;            js-ts-mode
;;            tsx-ts-mode
;;            scala-ts-mode
;;            typescript-ts-mode
;;            smithy-mode) . lsp-deferred)

(use-package lsp-bridge
  :ensure '( lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
             :build (:not compile))
  :custom
  (acm-enable-tabnine nil)
  (acm-enable-icon nil)
  (acm-enable-search-file-words nil)
  :custom
  (global-lsp-bridge-mode))

(use-package lsp-mode
  :defer t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-completion-mode)
         (lsp-completion-mode . conf/lsp-mode-completion-setup))
  :preface
  (defun conf/lsp-mode-completion-setup ()
    (setf (caadr ;; Pad before lsp modeline error info
				   (assq 'global-mode-string mode-line-misc-info))
				  " "))
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :custom
  (lsp-completion-provider :company-capf)
  (lsp-diagnostics-provider :flycheck)
  (lsp-keep-workspace-alive t)
  (lsp-idle-delay 0.5)
  (lsp-eldoc-render-all t)
  (lsp-modeline-workspace-status-enable t)
  (lsp-auto-execute-action nil)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color t)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-render-documentation nil)
  (lsp-signature-doc-lines 2)
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil)
  ;; Mine are better :
  (lsp-disabled-clients '(emmet-ls eslint))
  :init
  (setq lsp-keymap-prefix "SPC l")
  :config
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (lsp-enable-which-key-integration t))

(use-package lsp-metals
  :after lsp-mode
  :ensure t
  :custom
  (lsp-metals-server-args '("-Dmetals.allow-multiline-string-formatting=off"
                            "-Dmetals.enable-best-effort=true"
                            "-Dmetals.client=emacs"))
  (lsp-metals-enable-indent-on-paste t))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :hook ((lsp-mode emacs-lisp-mode) . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p 1)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-idle-delay 0.15))

(use-package eldoc-box
  :after eldoc
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t))

(use-package smartparens
  :demand t
  :bind ( ("M-\\" . #'sp-delete-backward-whitespace) )
  :hook ((clojure-mode emacs-lisp-mode lisp-mode) . smartparens-strict-mode)
  :init
  (setq sp-highlight-pair-overlay nil)
  :config
  (defun sp-delete-backward-whitespace ()
    "Delete whitespace backwards till it encounters a character."
    (interactive)
    (save-excursion
      (delete-region
       (point)
       (sp-backward-whitespace))))

  (require 'smartparens-config)
  (show-smartparens-global-mode)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (flycheck-error-list-mode . visual-line-mode))
  :bind ( :map
          flycheck-mode-map
          ("M-g d" . #'flycheck-list-errors)
          :map
          evil-normal-state-map
          ("]d" . #'next-error)
          ("[d" . #'previous-error))
  :preface
  (defvar-local conf/flycheck-local-cache nil)
  (defun conf/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker conf/flycheck-local-cache))
        (funcall fn checker property)))
  :custom
  (flycheck-emacs-lisp-load-path  'inherit)
  (flycheck-javascript-eslint-executable "eslint_d")
  :config
  (advice-add 'flycheck-checker-get :around 'conf/flycheck-checker-get)

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (cond ((derived-mode-p 'typescript-ts-base-mode)
                     (setq conf/flycheck-local-cache '((lsp . ((next-checkers . (javascript-biome)))))))))))

(use-package flycheck-deno
  :config
  (with-eval-after-load 'flycheck
    (flycheck-deno-setup)))

(use-package flycheck-kotlin
  :after 'lsp-mode
  :config
  (flycheck-kotlin-setup)
  (flycheck-add-next-checker 'lsp 'kotlin-ktlint))

(use-package flycheck-biomejs
  :ensure (:host github :repo "craneduck/flycheck-biomejs"))

(use-package eslintd-fix
  :hook ((js-ts-mode . eslintd-fix-mode)
         (typescript-ts-mode . eslintd-fix-mode)
         (tsx-ts-mode . eslintd-fix-mode)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package apheleia
  :config
  (--each '((zprint . ("zprint"))
            (scalafmt . ("scalafmt-native"
                         (when-let* ((project (project-current))
                                     (root (project-root project)))
                           (list "--config" (expand-file-name ".scalafmt.conf" root)))
                         "--stdin"))
            (biome . (npx "biome" "format" "--stdin-file-path" filepath)))
    (push it apheleia-formatters))

  (--each '((clojure-mode . zprint)
            (clojurescript-mode . zprint)
            (clojurec-mode . zprint)
            (typescript-ts-mode . biome)
            (tsx-ts-mode . biome)
            (scala-ts-mode . scalafmt))
    (push it apheleia-mode-alist)))

(use-package tramp
  :ensure nil
  :init
  (setq enable-remote-dir-locals t)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
               (list nil remote-shell "/bin/bash")))

(use-package which-func
  :ensure nil
  :hook (prog-mode . which-function-mode))

(use-package emmet-mode
  :hook ((js-base-mode typescript-ts-base-mode) . emmet-mode))

(use-package opam-switch-mode)

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package compile
  :ensure nil
  :custom
  (compilation-filter-hook '(ansi-color-compilation-filter
                             ansi-osc-compilation-filter))
  (compilation-scroll-output t))

(provide 'development)
;;; development.el ends here
