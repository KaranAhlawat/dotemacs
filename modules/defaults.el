;;; defaults.el --- Some sensible defaults I like -*- lexical-binding: t; -*-
;;; Commentary:
;;; Setup defaults
;;; Code:

;; Littering is bad. Stop it. Get some help.
(use-package no-littering
  :straight t
  :config
  (setq auto-save-file-name-transforms
        `((".*"
           ,(no-littering-expand-var-file-name "auto-save/")
           t))))

(setq global-auto-revert-non-file-buffers t)

;; Measure the startup time of  emacs
(defun conf/display-startup-time ()
  (message "Emacs loaded in %s with %d GCs."
	         (format "%.2f seconds"
		               (float-time
		                (time-subtract after-init-time before-init-time)))
	         gcs-done))

(add-hook 'emacs-startup-hook #'conf/display-startup-time)
(global-auto-revert-mode 1)

;; Try to tame the TAB
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default cursor-type 'box)
(setq tab-always-indent 'complete)

;; Shorten yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Save space in the kill ring
(setq kill-do-not-save-duplicates t)

;; Some scroll related settings
(setq auto-window-vscroll nil)
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(pixel-scroll-precision-mode)

;; Save the deleted files just in case to Trash
(setq delete-by-moving-to-trash t)

;; .dir-locals
(setq enable-local-variables t
      enable-dir-local-variables t)

(provide 'defaults)
;; defaults.el ends here
