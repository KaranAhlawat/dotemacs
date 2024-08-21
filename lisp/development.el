;;; development.el --- Making the money -*- lexical-binding: t; -*-
;;; Commentary:
;;; Sets up a great dev exp inside Emacs
;;; Code:

;; Magit, the magical git interface
(use-package transient
  :defer t)
(use-package magit
  :after transient
  :defer t
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (((heex-ts-mode
           elixir-ts-mode
           scala-ts-mode
           java-ts-mode
           js-ts-mode
           tsx-ts-mode
           typescript-ts-mode
           smithy-mode) . lsp-deferred)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . conf/lsp-mode-completion-setup))
  :preface
  (defun conf/lsp-mode-completion-setup ()
    (setf (caadr ;; Pad before lsp modeline error info
				   (assq 'global-mode-string mode-line-misc-info))
				  " "))
  :custom
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keep-workspace-alive t)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-semantic-tokens-enable-multiline-token-support nil)
  (lsp-eldoc-render-all t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 2)
  ;; Mine are better :)
  (lsp-disabled-clients '(emmet-ls eslint))
  :config
  (use-package lsp-elixir
    :ensure nil
    :after lsp-mode
    :custom
    (lsp-elixir-server-command '("/home/karan/repos/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("cs"
                                                            "launch"
                                                            "software.amazon.smithy:smithy-language-server:0.4.0"
                                                            "--ttl"
                                                            "1h"
                                                            "--repository"
                                                            "m2Local"
                                                            "--main-class"
                                                            "software.amazon.smithy.lsp.Main"
                                                            "--"
                                                            "0"))
                    :multi-root nil
                    :activation-fn (lsp-activate-on "smithy")
                    :initialization-options '((statusBarProvider . "show-message")
                                              (isHttpEnabled . t))
                    :server-id 'smithy-ls))

  (push '("\\.smithy$" . "smithy") lsp-language-id-configuration)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (use-package evil-core
    :ensure nil
    :after evil
    :config
    (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
    (add-hook 'lsp-mode-hook #'evil-normalize-keymaps))

  (lsp-enable-which-key-integration t))

(use-package lsp-metals
  :custom
  (lsp-metals-server-args
   '("-Dmetals.allow-multiline-string-formatting=on"
     "-Dmetals.enable-best-effort=true"
     "-Dmetals.client=emacs"))
  (lsp-metals-fallback-scala-version "3.3.3")
  (lsp-metals-enable-indent-on-paste t)
  (lsp-metals-enable-semantic-highlighting nil))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :hook lsp-mode
  :custom
  (eldoc-echo-area-use-multiline-p 1)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-idle-delay 0.15))

(use-package eldoc-box
  :after eldoc
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-max-pixel-width 500))

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
  :bind
  (:map
   flycheck-mode-map
   ("M-g d" . #'flycheck-list-errors))
  :custom
  (flycheck-javascript-eslint-executable "eslint_d"))

(use-package flycheck-deno
  :config
  (with-eval-after-load 'flycheck
    (flycheck-deno-setup)))

(use-package flycheck-kotlin
  :after 'lsp-mode
  :config
  (flycheck-kotlin-setup)
  (flycheck-add-next-checker 'lsp 'kotlin-ktlint))

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
                         "--stdin")))
    (push it apheleia-formatters))

  (--each '((clojure-mode . zprint)
            (clojurescript-mode . zprint)
            (clojurec-mode . zprint)
            (scala-ts-mode . scalafmt))
    (push it apheleia-mode-alist)))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; ANSI color in compilation buffer
(use-package ansi-color
  :ensure nil
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package tramp
  :ensure nil
  :init
  (setq enable-remote-dir-locals t)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
               (list nil "remote-shell" "/bin/bash")))

(use-package which-func
  :ensure nil
  :hook (prog-mode . which-function-mode))

(use-package opam-switch-mode)

(use-package emmet-mode
  :hook ((js-base-mode typescript-ts-base-mode) . emmet-mode))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(provide 'development)
;;; development.el ends here
