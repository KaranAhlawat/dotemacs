;;; setup-latex.el --- For latex editing is better  -*- lexical-binding: t; -*-
;;; Commentary:
;;; A place to configure everything relating to LaTeX
;;; Code:

;; AUCTEX
(use-package tex
  :ensure ( :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
            :pre-build (("make" "elpa"))
            :build (:not elpaca--compile-info) ;; Make will take care of this step
            :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
            :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :defer t
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . turn-on-auto-fill))
  :init
  (setq
   TeX-parse-self t
   TeX-auto-save t
   TeX-auto-untabify t
   TeX-auto-local ".auctex-auto"
   TeX-style-local ".auctex-style"
   TeX-source-correlate-mode t
   TeX-source-correlate-method 'synctex
   TeX-source-correlate-start-server t
   TeX-electric-sub-and-superscript t
   TeX-save-query nil
   TeX-view-program-selection '((output-pdf "Evince"))
   TeX-region ".auctex-region")
  (setq
   TeX-engine-alist '((default
                       "Tectonic" "tectonic -X compile -f plain %T"
                       "tectonic -X watch"
                       nil))
   LaTeX-command-style '(("" "%(latex)"))
   TeX-process-asynchronous t
   TeX-check-TeX nil
   TeX-engine 'default)

  (setq-default
   TeX-output-dir "build"
   TeX-master nil)

  :config
  (let ((tex-list (assoc "TeX" TeX-command-list))
        (latex-list (assoc "LaTeX" TeX-command-list)))
    (setf (cadr tex-list) "%(tex)"
          (cadr latex-list) "%l")))

(use-package cdlatex
  :after tex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind ( :map cdlatex-mode-map
          ("<tab>" . cdlatex-tab))
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-takeover-parenthesis nil))

(provide 'setup-latex)
;;; setup-latex.el ends here
