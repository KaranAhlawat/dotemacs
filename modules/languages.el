;;; languages.el --- Setup programming specific stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is just mostly language related stuff.
;;; Code:

;; Associate extensions with the correct tree-sitter mode and others
(dolist (pair '(("\\.py\\'" . python-ts-mode)
                ("\\.c\\'"  . c-ts-mode)
                ("\\.h\\'"  . c-ts-mode)
                ("\\.cpp\\'" . c++-ts-mode)
                ("\\.hpp\\'" . c++-ts-mode)
                ("\\.sh\\'" . bash-ts-mode)
                ("\\.js\\'" . js-ts-mode)
                ("\\.ts\\'" . typescript-ts-mode)
                ("\\.tsx\\'" . tsx-ts-mode)
                ("\\.cs\\'" . csharp-ts-mode)
                ("\\.java\\'" . java-ts-mode)
                ("\\.json\\'" . json-ts-mode)
                ("\\.css\\'" . css-ts-mode)))
  (push pair auto-mode-alist))

;; Cider for clojure
(use-package cider
  :straight t
  :mode "\\.clj\\'"
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-help-banner nil)
  (cider-shadow-cljs-command "npx shadow-cljs"))

(provide 'languages)
;;; languages.el ends here
