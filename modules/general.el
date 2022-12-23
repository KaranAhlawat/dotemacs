;;; general.el --- General packages and configurations -*- lexical-binding: t; -*-
;;; Commentary:
;;; General stuff that I don't think belongs to other modules but is
;;; too small to create it's own module
;;; Code:

;; Makes it easier to see where what is
(use-package rainbow-delimiters
  :straight t
  :hook (lisp-mode emacs-lisp-mode cider-mode cider-repl-mode clojure-mode))

;; Either act on the whole line or the current region
(use-package whole-line-or-region
  :straight t
  :config
  (whole-line-or-region-global-mode))

;; Some minor org tweaks
(use-package visual-fill-column
  :straight t
  :hook org-mode
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(provide 'general)
;;; general.el ends here
