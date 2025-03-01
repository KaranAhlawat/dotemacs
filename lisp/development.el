;;; development.el --- Making the money -*- lexical-binding: t; -*-
;;; Commentary:
;;; Sets up a great dev exp inside Emacs
;;; Code:

;; Magit, the magical git interface
;; (use-package compat)
(use-package with-editor)
(use-package transient)
(use-package magit
  :after compat
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-bridge
  :ensure '( lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
             :build (:not compile))
  :demand t
  :bind (("M-." . lsp-bridge-find-def)
         ("M-?" . lsp-bridge-find-references))
  :custom
  (acm-enable-tabnine nil)
  (acm-enable-icon nil)
  (acm-enable-search-file-words nil)
  (lsp-bridge-enable-hover-diagnostic t)
  :config
  (defun enable-lsp-bridge ()
    (interactive)
    (when-let* ((project (project-current))
                (project-root (nth 2 project)))
      (setq-local lsp-bridge-user-langserver-dir project-root
                  lsp-bridge-user-multiserver-dir project-root))
    (lsp-bridge-mode))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(ocaml-ts-mode . "ocamllsp"))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(scala-ts-mode . "metals"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'ocaml-ts-mode-hook)
  (add-to-list 'lsp-bridge-default-mode-hooks 'reason-ts-mode-hook)
  (add-to-list 'lsp-bridge-default-mode-hooks 'scala-ts-mode-hook)
  (global-lsp-bridge-mode))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :hook (emacs-lisp-mode . eldoc-mode)
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
          ("M-g d" . #'flycheck-list-errors))
  :custom
  (flycheck-emacs-lisp-load-path  'inherit)
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
