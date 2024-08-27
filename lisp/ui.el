;;; ui.el --- UI tweaks and customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; The file houses the UI changes I made to Emacs.
;;; Code:
(require 'cl-seq)
(require 'display-line-numbers)

(mapc #'disable-theme custom-enabled-themes)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Relative numbers to move around quicker
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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

(defvar conf/weight 'medium
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
        '((regular)
          (t
           :default-family "IBM Plex Mono"
           :default-weight medium
           :default-height 120
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil
           :fixed-pitch-family "IBM Plex Mono"
           :fixed-pitch-weight medium
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight medium
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "serif"
           :variable-pitch-height 130
           :variable-pitch-weight regular)))
  :config
  (fontaine-set-preset 'regular)
  (fontaine-mode))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold nil)
  (doom-oksolar-dark-brighter-comments t)
  (doom-oksolar-light-brighter-comments t)

  :config
  (with-eval-after-load 'org
    (doom-themes-org-config)
    (doom-themes-enable-org-fontification))
  (doom-themes-set-faces nil
    '(tooltip :inherit 'fixed-pitch)
    '(font-lock-comment-face :inherit 'italic))
  (load-theme 'doom-miramare t))

;; Cuz I may have the memory of a fish
(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

;; A more minimal modeline. Maybe someday I'll actually customize the defualt in-built one.
(use-package doom-modeline
  :demand t
  :custom-face
  (doom-modeline-buffer-file ((t (:weight ,conf/weight))))
  (doom-modeline-buffer-path ((t (:weight ,conf/weight))))
  :init
  (setq doom-modeline-icon nil)
  :config
  (column-number-mode)
  (doom-modeline-mode))

(use-package hide-mode-line
  :demand t
  :hook (completion-list-mode . hide-mode-line-mode))

(provide 'ui)
;;; ui.el ends here
