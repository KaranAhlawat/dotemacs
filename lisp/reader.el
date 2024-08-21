;;; general.el --- Reading ebooks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for reading ebooks (EPUBs and PDFs, mostly)
;;; Code:

(use-package doc-view
  :ensure nil
  :init
  (setq doc-view-mupdf-use-svg t))

(provide 'reader)
;;; reader.el ends here
