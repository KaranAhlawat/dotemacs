;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 50 1000 1000))

;; Load the latest version of a file in `load', if an extension is not
;; specified.
(setq load-prefer-newer noninteractive)

;; Customize native compilation
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-deferred-compilation t)
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

;; This essentially sets the size in pixels of the frame. Currently at
;; 1200x900.
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900))))


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
