;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 50 1000 1000))

;; Load the latest version of a file in `load', if an extension is not
;; specified.
(setq load-prefer-newer noninteractive)

;; Customize native compilation
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-automatic-native-compilation t)
(add-to-list 'native-comp-eln-load-path
	     (locate-user-emacs-file "eln-cache/"))

;; Shut down package.el in favor of straight.el While package.el is
;; built-in, and is now quite advanced, I am still not very
;; comfortable with how to use it, and don't find a reason to
;; currently put the time in to learn that. This will probably change
;; in the future, at which point this will be replaced. For now,
;; straight.el does everything I need.
(setq package-enable-at-startup nil)

;; Some general customizations (taken partly from
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/early-init.el)

(setq frame-resize-pixelwise t)
;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Set some properties on the default and initial frames
(setq default-frame-alist `((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (fullscreen . maximized)
                            (undecorated . t)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))
(setq initial-frame-alist `((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (fullscreen . maximized)
                            (undecorated . t)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

;; Reducing the GUI clutter. I recommend not doing this if just
;; getting started with emacs, as they help massively in
;; discoverability of the editor.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(setq initial-major-mode 'fundamental-mode)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves")))
(setq create-lockfiles nil)
