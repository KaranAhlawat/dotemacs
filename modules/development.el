;;; development.el --- Making the money -*- lexical-binding: t; -*-
;;; Commentary:
;;; Setups up a great dev exp inside Emacs
;;; Code:

;; Magit, the magical git interface
(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Now that eglot is in Emacs itself, that is a huge incentive to move
;; away from lsp-mode, at least for me.
(use-package eglot
  :straight nil
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.2)
  (eglot-confirm-server-initiated-edits nil)

  :config
  (dolist (mode '(c-ts-mode-hook
		  c++-ts-mode-hook
		  typescript-ts-mode-hook
		  tsx-ts-mode-hook
		  js-ts-mode-hook
		  python-ts-mode-hook
		  java-ts-mode-hook
		  csharp-ts-mode-hook))
    (add-hook mode 'eglot-ensure)))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :straight nil
  :hook eglot-ensure
  :custom
  (eldoc-echo-area-use-multiline-p 2))

(use-package smartparens
  :straight t
  :hook
  ((clojure-mode) . smartparens-strict-mode)
  :config
  (smartparens-global-mode))

;; Tweak flymake just a little bit
(use-package flymake
  :ensure nil
  :straight nil
  :hook prog-mode
  :bind (:map flymake-mode-map
              ("M-g d" . #'flymake-show-buffer-diagnostics)
              ("M-g M-d" . #'flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(provide 'development)
;;; development.el ends here
