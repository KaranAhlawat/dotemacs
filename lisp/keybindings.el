;;; keybindings.el --- Convenient keybindings for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Keybindings I had setup but forgot to add to yadm lmao
;;; Code:

(use-package evil-leader
  :demand t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" #'execute-extended-command
    "b" #'consult-buffer
    "s" #'save-buffer
    "k" #'kill-buffer
    "f" #'find-file
    "r" #'consult-recent-file
    "w" evil-window-map
    "qe" #'kill-emacs
    "qq" #'delete-frame
    "pp" #'project-switch-project
    "pb" #'consult-project-buffer
    "pk" #'project-kill-buffers
    "pf" #'project-find-file
    "pc" #'project-compile
    "pe" #'project-eshell
    "pg" #'project-find-regexp
    "af" #'apheleia-format-buffer
    "hv" #'helpful-variable
    "hf" #'helpful-callable
    "hk" #'helpful-key
    "hi" #'info
    "hp" #'eldoc-box-help-at-point
    "gi" #'consult-imenu
    "xe" #'eval-last-sexp
    "xp" #'pp-eval-last-sexp)
  (with-eval-after-load 'lsp-bridge
    (evil-leader/set-key
      "lf" #'lsp-bridge-code-format
      "lh" #'lsp-bridge-popup-documentation
      "la" #'lsp-bridge-code-action
      "lr" #'lsp-bridge-rename
      "ld" #'lsp-bridge-find-def
      "lg" #'lsp-bridge-find-references
      "ll" #'lsp-bridge-diagnostic-list
      "ln" #'lsp-bridge-diagnostic-jump-next
      "lp" #'lsp-bridge-diagnostic-jump-prev))
  (with-eval-after-load 'popper
    (evil-leader/set-key
      "``" #'popper-toggle
      "`q" #'popper-kill-latest-popup
      "`t" #'popper-toggle-type
      "`c" #'popper-cycle))
  (global-evil-leader-mode))

(use-package evil
  :demand t
  :bind (( "<escape>" . keyboard-escape-quit )
         :map evil-normal-state-map
         ( "U" . evil-redo )
         ( "M-." . nil )
         ( "C-." . nil )
         ( "<SPC>" . nil)
         ( "/" . ctrlf-forward-literal )
         ( "?" . ctrlf-backward-literal )
         ( "*"  . ctrlf-forward-symbol-at-point )
         ( "C-n" . evil-ex-nohighlight )
         ( "C-b" . evil-scroll-up )
         :map evil-visual-state-map
         ( "/" . ctrlf-forward-literal )
         ( "?" . ctrlf-backward-literal )
         ( "*"  . ctrlf-forward-symbol-at-point )
         :map evil-insert-state-map
         ( "C-y" . nil ))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-want-fine-undo t)
  (setq evil-symbol-word-search nil)
  (setq evil-default-cursor 't)
  (setq evil-insert-state-cursor 't)
  :config
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :bind ( :map evil-normal-state-map
          ( "f"  . #'evil-avy-goto-char-in-line )
          ( "gl" . #'evil-avy-goto-line )
          ( "gs"  . #'evil-avy-goto-char-timer )
          :map evil-visual-state-map
          ( "f" . #'evil-avy-goto-char-in-line ))
  :init
  (setq evil-want-integration t)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :hook ((clojure-mode emacs-lisp-mode lisp-mode) . evil-cleverparens-mode)
  :config
  (push 'evil-cp-change evil-change-commands))

(use-package avy)

(provide 'keybindings)
;;; keybindings.el ends here
