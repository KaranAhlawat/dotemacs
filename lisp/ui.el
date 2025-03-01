;;; ui.el --- UI tweaks and customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; The file houses the UI changes I made to Emacs.
;;; Code:
(require 'cl-seq)
(require 'display-line-numbers)

(mapc #'disable-theme custom-enabled-themes)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Hide the line numbering in certain modes as well
(defun conf/disable-line-numbers-in-mode ()
  "Disable `display-line-numbers-mode' in a major-mode."
  (display-line-numbers-mode -1))

(defmacro conf/daemon-frame-hook! (body)
  "Add BODY to `after-make-frame-functions' properly."
  `(if (daemonp)
       (add-to-list 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame
                          frame
                        ,body)))
     ,body))

(dolist (mode '(org-mode-hook eshell-mode-hook))
  (add-hook mode #'conf/disable-line-numbers-in-mode))

(defvar conf/weight 'regular
  "Weight used for faces throughout config.")

(use-package fontaine
  :demand t
  :bind ("C-c f" . #'fontaine-set-preset)
  :init
  (setq x-underline-at-descent-line t)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file
                                    "fontaine-latest-state.eld"))
  (setq fontaine-presets
        `((regular)
          (t
           :default-family "Rec Mono Duotone"
           :default-weight ,conf/weight
           :default-height 110
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil
           :fixed-pitch-family "Rec Mono Duotone"
           :fixed-pitch-weight ,conf/weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight regular
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "serif"
           :variable-pitch-height 120
           :variable-pitch-weight regular)))
  (fontaine-set-preset 'regular)
  (fontaine-mode))

(use-package standard-themes
  :config
  (load-theme 'standard-dark t nil))

;; Cuz I may have the memory of a fish
(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package hide-mode-line
  :demand t
  :hook (completion-list-mode . hide-mode-line-mode))

(provide 'ui)
;;; ui.el ends here
