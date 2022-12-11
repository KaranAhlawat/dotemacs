;;; completion.el --- Completions for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This packages sets up various completion providers
;;; Code:

;; Learn from your history, so you are bound to repeat it?
(use-package savehist
  :straight t
  :config
  (savehist-mode))

;; Those quick annotations are really helpful
(use-package marginalia
  :straight t
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil))
  (marginalia-align 'right)
  :config
  (marginalia-mode))

;; fzf-based fuzzy matching for emacs
(use-package fussy
  :straight t
  :custom
  (completion-styles '(fussy))
  :config
  (setq fussy-filter-fn 'fussy-filter-flex)
  (setq fussy-use-cache t)
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)

  ;; Corfu integration
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil)))
  
  ;; For example, `project-find-file' uses 'project-files which uses
  ;; substring completion by default. Set to `nil' to make sure it's using
  ;; flx.
  (setq completion-category-defaults nil
        completion-category-overrides nil)

  ;; `eglot' integration - now that it's built-in
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

;;; use `fzf-native' as the backing implementation
(use-package fzf-native
  :after fussy
  :ensure t
  :straight
  (fzf-native
   :repo "dangduc/fzf-native"
   :host github
   :files (:defaults "bin"))
  :init
  (setq fussy-score-fn 'fussy-fzf-native-score)
  :config
  (fzf-native-load-dyn))

;; Here, we're advising `fido' to use fussy for completion and filtering
(use-package icomplete
  :ensure nil
  :straight nil
  :config
  (defun conf/fussy-fido-setup ()
    "Use `fussy' with `fido-mode'."
    (setq-local completion-styles '(fussy basic)))
  (advice-add 'icomplete--fido-mode-setup :after 'conf/fussy-fido-setup)
  (setq icomplete-tidy-shadowed-file-names t
        icomplete-show-matches-on-no-input t
        icomplete-compute-delay 0
        icomplete-delay-completions-threshold 50)
  ;; Or `fido-mode'.
  (fido-vertical-mode))

;; Tempel for expansion - May remove this in the future as I don't use it alot.
(use-package tempel
  :straight t
  :bind (:map tempel-map
              ("TAB" . #'tempel-next))
  :config
  (defun conf/tempel-setup-capf ()
    "Setup tempel capf function."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  (with-eval-after-load 'clojure-mode
    (define-key clojure-mode-map (kbd "C-t") 'tempel-expand))
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-t") 'tempel-expand))
  :hook
  (org-mode . conf/tempel-setup-capf)
  (clojure-mode . conf/tempel-setup-capf))

;; Corfu for completion at point popup
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-on-exact-match nil)

  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert))
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode)))
  
  (global-corfu-mode 1))

(provide 'completions)
;;; completions.el ends here
