;;; languages.el --- Setup programming specific stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is just mostly language related stuff.
;;; Code:

(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-font-lock-level 4)
  :config
  (seq-do (lambda (it)
            (push it treesit-language-source-alist))
          '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3"   "tsx/src"))
            (css . ("https://github.com/tree-sitter/tree-sitter-css"))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
            (json . ("https://github.com/tree-sitter/tree-sitter-json"))
            (python . ("https://github.com/tree-sitter/tree-sitter-python"))
            (go . ("https://github.com/tree-sitter/tree-sitter-go"))
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
            (java . ("https://github.com/tree-sitter/tree-sitter-java"))
            (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
            (scala . ("https://github.com/tree-sitter/tree-sitter-scala" "v0.20.2"))
            (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "v0.21.2" "grammars/ocaml/src"))
            (ocaml-interface . ("https://github.com/tree-sitter/tree-sitter-ocaml" "v0.21.2" "grammars/interface/src"))
            (php . ("https://github.com/tree-sitter/tree-sitter-php" nil "php/src"))
            (fsharp . ("https://github.com/ionide/tree-sitter-fsharp"))))

  (seq-do (lambda (it)
            (push it major-mode-remap-alist))
          '((python-mode . python-ts-mode)
            (c-mode . c-ts-mode)
            (csharp-mode . csharp-ts-mode)
            (c++-mode . c++-ts-mode)
            (javascript-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (java-mode . java-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (php-mode . php-ts-mode)
            (shell-script-mode . bash-ts-mode))))

(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight"))

;; Cider for clojure
(use-package cider
  :straight t
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-help-banner nil)
  (cider-shadow-cljs-command "npx shadow-cljs"))

;; Web Stuff
(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode))
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2))

(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2))

(use-package python
  :straight (:type built-in)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-indent-guess-indent-offset-verbose nil))

(use-package micromamba
  :straight t)

(use-package css-mode
  :straight (:type built-in)
  :custom
  (css-indent-offset 2))

(use-package java-ts-mode
  :straight (:type built-in)
  :custom
  (java-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.jsx\\'" . js-jsx-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; Scala TS (local)
(use-package scala-ts-mode
  :mode "\\.scala\\'"
  :straight (:local-repo "/home/karan/repos/scala-ts-mode")
  :init
  (setq scala-ts-indent-offset 4))

(use-package fsharp-ts-mode
  :mode "\\.fs\\'"
  :straight (:local-repo "/home/karan/repos/fsharp-ts-mode")
  :init
  (setq fsharp-ts-indent-offset 4))

(use-package reason-ts-mode
  :mode "\\.re\\'"
  :straight (:local-repo "/home/karan/repos/reason-ts-mode"))

(use-package yaml-ts-mode
  :straight (:type built-in))

(use-package tuareg
  :straight t)

(use-package ocaml-ts-mode
  :straight ( :type git
              :host github
              :repo "terrateamio/ocaml-ts-mode" )
  :after tuareg
  :mode "\\.ml\\'")

(use-package smithy-mode
  :straight t)

(provide 'languages)
;;; languages.el ends here