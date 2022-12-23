;;; ui.el --- UI tweaks and customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; The file houses the UI changes I made to Emacs.
;;; Code:

;; Disable all other themes first
(mapc #'disable-theme custom-enabled-themes)

;; Relative line numbers to move around quicker
(require 'display-line-numbers)
(global-hl-line-mode 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Hide the line numbering in certain modes as well
(defun conf/disable-line-numbers-in-mode ()
  "Disable `dispaly-line-numbers-mode' in a major-mode."
  (display-line-numbers-mode -1))

(dolist (mode '(org-mode-hook eshell-mode-hook))
  (add-hook mode #'conf/disable-line-numbers-in-mode))

;; Switch off the visible bell, it's distracting to me. As well as the
;; blinking cursor
(setq visible-bell nil)
(blink-cursor-mode nil)

;; Setup fonts with Fontaine package
(use-package fontaine
  :straight t
  
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state-file.eld"))

  (fontaine-presets
   '((small
      :default-height 130)
     (regular
      :default-height 150)
     (large
      :default-weight semilight
      :default-height 170
      :bold-weight extrabold)
     (extra-large
      :default-weight semilight
      :default-height 190
      :bold-weight extrabold)
     (t
      :default-family "Input Mono Narrow"
      :default-weight regular
      :variable-pitch-weight regular
      :variable-pitch-height 1.0
      :bold-family nil ; use whatever the underlying face has
      :bold-weight bold
      :italic-family nil
      :italic-slant italic
      :line-spacing nil)))

  :init
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset)
			   'regular)))

;; And now for the themes

;; In my opinion, Protesilaos Stavrou has some of the best theme
;; packages in Emacs. These would be the built-in Modus Themes, the EF
;; Themes and now the Standard Themes. Currently I'm just trying out
;; how Standard Themes feel and look.

;; TODO 9/12/2022 - Play around with the values to find something I
;; may like better.
(use-package standard-themes
  :straight t
  
  :custom
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-mixed-fonts t)
  (standard-themes-variable-pitch-ui t)
  (standard-themes-mode-line-accented t)
  (standard-themes-fringes 'subtle)
  (standard-themes-links '(neutral-underline))
  (standard-themes-region '(no-extend neutral intense))
  (standard-themes-prompts '(bold italic))
  (standard-themes-headings
   '((0 . (variable-pitch light 1.9))
     (1 . (variable-pitch light 1.8))
     (2 . (variable-pitch light 1.7))
     (3 . (variable-pitch semilight 1.6))
     (4 . (variable-pitch semilight 1.5))
     (5 . (variable-pitch 1.4))
     (6 . (variable-pitch 1.3))
     (7 . (variable-pitch 1.2))
     (t . (variable-pitch 1.1))))
  
  :config
  (load-theme 'standard-light :no-confirm))

;; Cuz I may have the memory of a fish
(use-package which-key
  :straight t
  
  :custom
  (which-key-idle-delay 0.5)
  
  :config
  (which-key-mode))

;; A more minimal modeline. Maybe someday I'll actually customize the defualt in-built one.
(use-package mood-line
  :straight t
  
  :custom
  (mood-line-show-eol-style t)
  (mood-line-show-cursor-point t)
  (mood-line-show-encoding-information t)
  
  :config
  (mood-line-mode))

(provide 'ui)
;;; ui.el ends here
