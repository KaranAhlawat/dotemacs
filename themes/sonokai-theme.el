;;; sonokai-theme.el --- Sonokai Theme  -*- lexical-binding: t; -*-
;;; Commentary:

;; Sonokai Palette
;;          reg     dim     bright  alt
;; yellow:  #E7C664 #BAA741 #FFDB52 #C4A64F
;; orange:  #F39660 #C2733D #FFB075
;; red:     #FC5D7C #C63958 #FF81A0 #FF6077
;; magenta: #DC8CC3 #D67AB9 #DF96C8
;; blue:    #76CCE0 #4D9EAD #9EDDF4
;; green:   #9ED072 #7BAA56 #B8E08C #A7DF78
;; cyan:    #85D3F2 #5AA8D9 #A1E8FF
;; violet:  #B39DF3 #8A74D1 #D1BAFF
;; gray:    #7F8490 #595E6B #9FA4AF
;; black:   #181819 #101010 #282828
;; white:   #E2E2E3 #BfBFBF #F0F0F0
;; bg:      #2C2E34 #222327 #33353F
;; bg-alt:  #3B3E48 #363944 #414550
;; d-red:   #55393D #482F32 #906369
;; d-blue:  #354157 #2B3649 #5B6F92
;; d-green: #394634 #2F3B2B #6D8464

;;; Code:

(unless (>= emacs-major-version 27)
  (error "The sonokai theme requires Emacs 27 or later!"))

(deftheme sonokai "The Sonokai colour theme.")

(defgroup sonokai nil
  "Sonokai theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom sonokai-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'sonokai)

(defcustom sonokai-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'sonokai)

(defcustom sonokai-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'sonokai
  :package-version "3.5.1")

(defcustom sonokai-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'sonokai)

(defcustom sonokai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'sonokai)

(defcustom sonokai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'sonokai)

(defcustom sonokai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'sonokai)

(defcustom sonokai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'sonokai)

;; Primary colors
(defcustom sonokai-yellow "#E7C664"
  "Primary colors - yellow."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-orange "#F39660"
  "Primary colors - orange."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-red "#FC5D7C"
  "Primary colors - red."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-magenta "#DC8CC3"
  "Primary colors - magenta."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-blue "#76CCE0"
  "Primary colors - blue."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-green "#9ED072"
  "Primary colors - green."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-cyan "#85D3F2"
  "Primary colors - cyan."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-violet "#B39DF3"
  "Primary colors - violet."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-gray "#7F8490"
  "Primary colors - gray."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-white "#E2E2E3"
  "Primary colors - white."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-black "#181819"
  "Primary colors - black."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-foreground "#E2E2E3"
  "Adaptive colors - foreground."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-background "#2C2E34"
  "Adaptive colors - background."
  :type 'string
  :group 'sonokai)

(defcustom sonokai-background-alt "#3B3E48"
  "Adaptive colors - background alt."
  :type 'string
  :group 'sonokai)

(let* (;; Variable pitch
       (sonokai-pitch (if sonokai-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (sonokai-class '((class color) (min-colors 257)))

       ;; Functionality specific colors
       (sonokai-diff-blue-base      "#354157")
       (sonokai-diff-blue-alt       "#2B3649")
       (sonokai-diff-blue-emphasis  "#5B6F92")
       (sonokai-diff-green-base     "#394634")
       (sonokai-diff-green-alt      "#2F3B2B")
       (sonokai-diff-green-emphasis "#6D8464")
       (sonokai-diff-red-base       "#55393D")
       (sonokai-diff-red-alt        "#482F32")
       (sonokai-diff-red-emphasis   "#906369")

       ;; Darker and lighter accented colors
       (sonokai-yellow-d       "#BAA741")
       (sonokai-yellow-l       "#FFDB52")
       (sonokai-orange-d       "#C2733D")
       (sonokai-orange-l       "#FFB075")
       (sonokai-red-d          "#C63958")
       (sonokai-red-l          "#FF81A0")
       (sonokai-magenta-d      "#D67AB9")
       (sonokai-magenta-l      "#DF96C8")
       (sonokai-violet-d       "#8A74D1")
       (sonokai-violet-l       "#D1BAFF")
       (sonokai-blue-d         "#4D9EAD")
       (sonokai-blue-l         "#9EDDF4")
       (sonokai-cyan-d         "#5AA8D9")
       (sonokai-cyan-l         "#A1E8FF")
       (sonokai-green-d        "#7BAA56")
       (sonokai-green-l        "#B8E08C")
       (sonokai-gray-d         "#595E6B")
       (sonokai-gray-l         "#9FA4AF")
       (sonokai-black-d        "#101010")
       (sonokai-black-l        "#282828")
       (sonokai-white-d        "#BFBFBF")
       (sonokai-white-l        "#F0F0F0")
       (sonokai-background-d   "#222327")
       (sonokai-background-l   "#33353F")
       (sonokai-bg-alt-d       "#363944")
       (sonokai-bg-alt-l       "#414550")

       ;; Adaptive colors
       (sonokai-comments       sonokai-gray)
       (sonokai-emphasis       sonokai-white)
       (sonokai-line-number    sonokai-gray-d)
       (sonokai-highlight      sonokai-bg-alt-l)
       (sonokai-highlight-alt  sonokai-background-alt)
       (sonokai-highlight-line sonokai-bg-alt-d)

       ;; Adaptive higher/lower contrast accented colors
       (sonokai-foreground-hc  sonokai-black-d)
       (sonokai-foreground-lc  sonokai-black-l)

       ;; High contrast colors
       (sonokai-yellow-hc      sonokai-yellow-d)
       (sonokai-yellow-hc-alt  "#C4A64F")
       (sonokai-yellow-lc      sonokai-yellow-l)
       (sonokai-orange-hc      sonokai-orange-d)
       (sonokai-orange-lc      sonokai-orange-l)
       (sonokai-red-hc         sonokai-red-d)
       (sonokai-red-hc-alt     "#D84A5F")
       (sonokai-red-lc         sonokai-red-l)
       (sonokai-magenta-hc     sonokai-magenta-d)
       (sonokai-magenta-lc     sonokai-magenta-l)
       (sonokai-violet-hc      sonokai-violet-d)
       (sonokai-violet-lc      sonokai-violet-l)
       (sonokai-blue-hc        sonokai-blue-d)
       (sonokai-blue-lc        sonokai-blue-l)
       (sonokai-cyan-hc        sonokai-cyan-d)
       (sonokai-cyan-lc        sonokai-cyan-l)
       (sonokai-green-hc       sonokai-green-d)
       (sonokai-green-hc-alt   "#A7DF78")
       (sonokai-green-lc       sonokai-green-l)

       ;; Distinct fringe
       (sonokai-fringe-bg (if sonokai-distinct-fringe-background
                              sonokai-gray
                            sonokai-background))

       ;; Definitions for terminals that do not support 256 colors
       (sonokai-256-class '((class color) (min-colors 89)))

       ;; Functionality specific colors
       (sonokai-256-diff-blue-base      sonokai-diff-blue-base)
       (sonokai-256-diff-blue-alt       sonokai-diff-blue-alt)
       (sonokai-256-diff-blue-emphasis  sonokai-diff-blue-emphasis)
       (sonokai-256-diff-green-base     sonokai-diff-green-base)
       (sonokai-256-diff-green-alt      sonokai-diff-green-alt)
       (sonokai-256-diff-green-emphasis sonokai-diff-green-emphasis)
       (sonokai-256-diff-red-base       sonokai-diff-red-base)
       (sonokai-256-diff-red-alt        sonokai-diff-red-alt)
       (sonokai-256-diff-red-emphasis   sonokai-diff-red-emphasis)

       ;; Primary colors
       (sonokai-256-yellow         sonokai-yellow)
       (sonokai-256-orange         sonokai-orange)
       (sonokai-256-red            sonokai-red)
       (sonokai-256-magenta        sonokai-magenta)
       (sonokai-256-violet         sonokai-violet)
       (sonokai-256-blue           sonokai-blue)
       (sonokai-256-cyan           sonokai-cyan)
       (sonokai-256-green          sonokai-green)
       (sonokai-256-gray           sonokai-gray)

       ;; Darker and lighter accented colors
       (sonokai-256-yellow-d       sonokai-yellow-d)
       (sonokai-256-yellow-l       sonokai-yellow-l)
       (sonokai-256-orange-d       sonokai-orange-d)
       (sonokai-256-orange-l       sonokai-orange-l)
       (sonokai-256-red-d          sonokai-red-d)
       (sonokai-256-red-l          sonokai-red-l)
       (sonokai-256-magenta-d      sonokai-magenta-d)
       (sonokai-256-magenta-l      sonokai-magenta-l)
       (sonokai-256-violet-d       sonokai-violet-d)
       (sonokai-256-violet-l       sonokai-violet-l)
       (sonokai-256-blue-d         sonokai-blue-d)
       (sonokai-256-blue-l         sonokai-blue-l)
       (sonokai-256-cyan-d         sonokai-cyan-d)
       (sonokai-256-cyan-l         sonokai-cyan-l)
       (sonokai-256-green-d        sonokai-green-d)
       (sonokai-256-green-l        sonokai-green-l)
       (sonokai-256-gray-d         sonokai-gray-d)
       (sonokai-256-gray-l         sonokai-gray-l)
       (sonokai-256-black-d        sonokai-black-d)
       (sonokai-256-black-l        sonokai-black-l)
       (sonokai-256-white-d        sonokai-white-d)
       (sonokai-256-white-l        sonokai-white-l)

       ;; Adaptive colors
       (sonokai-256-foreground     sonokai-foreground)
       (sonokai-256-background     sonokai-background)
       (sonokai-256-background-d   sonokai-background-d)
       (sonokai-256-background-l   sonokai-background-l)
       (sonokai-256-background-alt sonokai-background-alt)
       (sonokai-256-bg-alt-d       sonokai-bg-alt-d)
       (sonokai-256-bg-alt-l       sonokai-bg-alt-l)
       (sonokai-256-comments       sonokai-comments)
       (sonokai-256-emphasis       sonokai-emphasis)
       (sonokai-256-line-number    sonokai-line-number)
       (sonokai-256-highlight      sonokai-highlight)
       (sonokai-256-highlight-alt  sonokai-highlight-alt)
       (sonokai-256-highlight-line sonokai-highlight-line)

       ;; Adaptive higher/lower contrast accented colors
       (sonokai-256-foreground-hc  sonokai-black-d)
       (sonokai-256-foreground-lc  sonokai-black-l)

       ;; High contrast colors
       (sonokai-256-yellow-hc      sonokai-256-yellow-d)
       (sonokai-256-yellow-lc      sonokai-256-yellow-l)
       (sonokai-256-orange-hc      sonokai-256-orange-d)
       (sonokai-256-orange-lc      sonokai-256-orange-l)
       (sonokai-256-red-hc         sonokai-256-red-d)
       (sonokai-256-red-lc         sonokai-256-red-l)
       (sonokai-256-magenta-hc     sonokai-256-magenta-d)
       (sonokai-256-magenta-lc     sonokai-256-magenta-l)
       (sonokai-256-violet-hc      sonokai-256-violet-d)
       (sonokai-256-violet-lc      sonokai-256-violet-l)
       (sonokai-256-blue-hc        sonokai-256-blue-d)
       (sonokai-256-blue-lc        sonokai-256-blue-l)
       (sonokai-256-cyan-hc        sonokai-256-cyan-d)
       (sonokai-256-cyan-lc        sonokai-256-cyan-l)
       (sonokai-256-green-hc       sonokai-256-green-d)
       (sonokai-256-green-lc       sonokai-256-green-l)

       ;; Distinct fringe
       (sonokai-256-fringe-bg (if sonokai-distinct-fringe-background
                                  sonokai-256-gray
                                sonokai-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'sonokai

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(font-lock-comment-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(font-lock-constant-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(font-lock-doc-face
     ((,sonokai-class (:foreground ,(if sonokai-doc-face-as-comment
                                        sonokai-comments
                                      sonokai-yellow)))
      (,sonokai-256-class (:foreground ,(if sonokai-doc-face-as-comment
                                            sonokai-256-comments
                                          sonokai-256-yellow)))))

   `(font-lock-function-name-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(font-lock-keyword-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight normal))))

   `(font-lock-negation-char-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(font-lock-preprocessor-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :weight normal))))

   `(font-lock-string-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(font-lock-type-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :italic nil))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :italic nil))))

   `(font-lock-variable-name-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(font-lock-warning-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :weight bold
                                       :italic t
                                       :underline t))))

   `(c-annotation-face
     ((,sonokai-class (:inherit font-lock-constant-face))
      (,sonokai-256-class (:inherit font-lock-constant-face))))

   ;; general colouring
   `(button ((t (:underline t))))

   `(tab-bar
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-background-d))
      (,sonokai-256-class (:foreground ,sonokai-foreground
                                       :background ,sonokai-256-background-d))))

   `(tab-bar-tab
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-background))))

   `(tab-bar-tab-inactive
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background-d))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background-d))))

   `(tab-bar-tab-group-current
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-background))))

   `(tab-bar-tab-group-inactive
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background))))

   `(tab-bar-tab-ungrouped
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background))))

   `(default
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-background))))

   `(highlight
     ((,sonokai-class (:background ,sonokai-highlight))
      (,sonokai-256-class (:background ,sonokai-256-highlight))))

   `(lazy-highlight
     ((,sonokai-class (:inherit highlight
                                :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:inherit highlight
                                    :background ,sonokai-256-highlight-alt))))

   `(region
     ((,sonokai-class (:inherit highlight
                                :background ,sonokai-highlight))
      (,sonokai-256-class (:inherit highlight
                                    :background ,sonokai-256-highlight))))

   `(secondary-selection
     ((,sonokai-class (:inherit region
                                :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:inherit region
                                    :background ,sonokai-256-highlight-alt))))

   `(shadow
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(match
     ((,sonokai-class (:background ,sonokai-green
                                   :foreground ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-green
                                       :foreground ,sonokai-256-background
                                       :weight bold))))

   `(cursor
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-foreground
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-foreground
                                       :inverse-video t))))

   `(mouse
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-foreground
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-foreground
                                       :inverse-video t))))

   `(escape-glyph
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(escape-glyph-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(fringe
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-fringe-bg))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-fringe-bg))))

   `(link
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline t
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline t
                                       :weight bold))))

   `(link-visited
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :underline t
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :underline t
                                       :weight normal))))

   `(success
     ((,sonokai-class (:foreground ,sonokai-green ))
      (,sonokai-256-class (:foreground ,sonokai-256-green ))))

   `(warning
     ((,sonokai-class (:foreground ,sonokai-yellow ))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow ))))

   `(error
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(eval-sexp-fu-flash
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-green))))

   `(eval-sexp-fu-flash-error
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-red))))

   `(trailing-whitespace
     ((,sonokai-class (:background ,sonokai-red))
      (,sonokai-256-class (:background ,sonokai-256-red))))

   `(vertical-border
     ((,sonokai-class (:foreground ,sonokai-gray))
      (,sonokai-256-class (:foreground ,sonokai-256-gray))))

   `(menu
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-background))))

   `(minibuffer-prompt
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(mode-line
     ((,sonokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,sonokai-emphasis
                                      :background ,sonokai-highlight
                                      :box (:line-width (1 . 1)
                                                        :color ,sonokai-gray
                                                        )))
      (,sonokai-256-class (:inverse-video unspecified
                                          :underline unspecified
                                          :foreground ,sonokai-256-foreground
                                          :background ,sonokai-256-background
                                          :box (:line-width (1 . 1)
                                                            :color ,sonokai-256-highlight
                                                            )))))

   `(powerline-active1
     ((,sonokai-class (:background ,sonokai-gray-d))
      (,sonokai-256-class (:background ,sonokai-256-gray-d))))

   `(powerline-active2
     ((,sonokai-class (:background ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-background))))


   `(mode-line-inactive
     ((,sonokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,sonokai-comments
                                      :background ,sonokai-background
                                      :box (:line-width (1 . 1)
                                                        :color ,sonokai-gray
                                                        )))
      (,sonokai-256-class (:inverse-video unspecified
                                          :underline unspecified
                                          :foreground ,sonokai-256-comments
                                          :background ,sonokai-256-background
                                          :box (:line-width (1 . 1)
                                                            :color ,sonokai-256-gray
                                                            )))))

   `(powerline-inactive1
     ((,sonokai-class (:background ,sonokai-gray-d))
      (,sonokai-256-class (:background ,sonokai-256-gray-d))))

   `(powerline-inactive2
     ((,sonokai-class (:background ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-background))))

   ;; powerline-evil
   `(powerline-evil-base-face
     ((,sonokai-class (:foreground ,sonokai-foreground)))
     (,sonokai-256-class (:foreground ,sonokai-256-foreground)))

   `(powerline-evil-emacs-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-violet)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-violet))))

   `(powerline-evil-normal-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-blue)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-blue))))

   `(powerline-evil-insert-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-green)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-green))))

   `(powerline-evil-motion-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-magenta)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-magenta))))

   `(powerline-evil-visual-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-orange)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-orange))))

   `(powerline-evil-replace-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-red)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-red))))

   `(powerline-evil-operator-face
     ((,sonokai-class (:inherit powerline-evil-base-face
                                :foreground ,sonokai-cyan)))
     ((,sonokai-256-class (:inherit powerline-evil-base-face
                                    :foreground ,sonokai-256-cyan))))

   ;; telephone-line evil
   `(telephone-line-evil
     ((,sonokai-class (:inherit mode-line
                                :foreground ,sonokai-foreground))
      (,sonokai-256-class (:inherit mode-line
                                    :foreground ,sonokai-256-foreground))))

   `(telephone-line-evil-emacs
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-violet))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-violet))))

   `(telephone-line-evil-normal
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-blue))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-blue))))

   `(telephone-line-evil-insert
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-green))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-green))))

   `(telephone-line-evil-replace
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-red))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-red))))

   `(telephone-line-evil-visual
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-orange))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-orange))))

   `(telephone-line-evil-operator
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-cyan))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-cyan))))

   `(telephone-line-evil-motion
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-magenta))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-magenta))))

   `(telephone-line-evil-god
     ((,sonokai-class (:inherit telephone-line-evil
                                :foreground ,sonokai-background
                                :background ,sonokai-white-d))
      (,sonokai-256-class (:inherit telephone-line-evil
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-white-d))))

   ;; header-line
   `(header-line
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-highlight
                                   :box (:color ,sonokai-gray
                                                :line-width (1 . 1)
                                                )))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-highlight
                                       :box (:color ,sonokai-256-gray
                                                    :line-width (1 . 1)
                                                    )))))

   ;; cua
   `(cua-global-mark
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-background))))

   `(cua-rectangle
     ((,sonokai-class (:inherit region))
      (,sonokai-256-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,sonokai-class (:inherit secondary-selection))
      (,sonokai-256-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   ;; dired
   `(dired-directory
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(dired-flagged
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(dired-header
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-background
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background
                                       :inherit bold))))

   `(dired-ignored
     ((,sonokai-class (:inherit shadow))
      (,sonokai-256-class (:inherit shadow))))

   `(dired-mark
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(dired-marked
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :inherit bold))))

   `(dired-perm-write
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :underline t))))

   `(dired-symlink
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :slant italic))))

   `(dired-warning
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-blue))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-blue))))

   `(dropdown-list-selection-face
     ((,sonokai-class (:background ,sonokai-green
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-green
                                       :foreground ,sonokai-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,sonokai-class (:inherit ecb-history-bucket-node-face
                                :foreground ,sonokai-yellow))
      (,sonokai-256-class (:inherit ecb-history-bucket-node-face
                                    :foreground ,sonokai-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,sonokai-class (:inherit ecb-directories-general-face
                                :foreground ,sonokai-foreground))
      (,sonokai-256-class (:inherit ecb-directories-general-face
                                    :foreground ,sonokai-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,sonokai-class (:inherit ecb-history-general-face
                                :foreground ,sonokai-comments))
      (,sonokai-256-class (:inherit ecb-history-general-face
                                    :foreground ,sonokai-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,sonokai-class (:inherit ecb-directories-general-face
                                :foreground ,sonokai-comments))
      (,sonokai-256-class (:inherit ecb-directories-general-face
                                    :foreground ,sonokai-256-comments))))

   `(ecb-bucket-node-face
     ((,sonokai-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,sonokai-blue))
      (,sonokai-256-class (:inherit ecb-default-general-face
                                    :weight normal
                                    :foreground ,sonokai-256-blue))))

   `(ecb-tag-header-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,sonokai-class (:inherit ecb-analyse-general-face
                                :foreground ,sonokai-green))
      (,sonokai-256-class (:inherit ecb-analyse-general-face
                                    :foreground ,sonokai-256-green))))

   `(ecb-directories-general-face
     ((,sonokai-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,sonokai-256-class (:inherit ecb-default-general-face
                                    :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,sonokai-class (:inherit ecb-methods-general-face
                                :foreground ,sonokai-cyan))
      (,sonokai-256-class (:inherit ecb-methods-general-face
                                    :foreground ,sonokai-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(ecb-tree-guide-line-face
     ((,sonokai-class (:inherit ecb-default-general-face
                                :foreground ,sonokai-gray
                                :height 1.0))
      (,sonokai-256-class (:inherit ecb-default-general-face
                                    :foreground ,sonokai-256-gray
                                    :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,sonokai-class (:foreground ,sonokai-emphasis))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis))))

   `(ee-category
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(ee-link
     ((,sonokai-class (:inherit link))
      (,sonokai-256-class (:inherit link))))

   `(ee-link-visited
     ((,sonokai-class (:inherit link-visited))
      (,sonokai-256-class (:inherit link-visited))))

   `(ee-marked
     ((,sonokai-class (:foreground ,sonokai-magenta
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :weight bold))))

   `(ee-omitted
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(ee-shadow
     ((,sonokai-class (:inherit shadow))
      (,sonokai-256-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(grep-error-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold
                                       :underline t))))

   `(grep-hit-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(grep-match-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   ;; isearch
   `(isearch
     ((,sonokai-class (:inherit region
                                :foreground ,sonokai-background
                                :background ,sonokai-yellow))
      (,sonokai-256-class (:inherit region
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-yellow))))

   `(isearch-fail
     ((,sonokai-class (:inherit isearch
                                :foreground ,sonokai-red
                                :background ,sonokai-background
                                :bold t))
      (,sonokai-256-class (:inherit isearch
                                    :foreground ,sonokai-256-red
                                    :background ,sonokai-256-background
                                    :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background
                                   :inverse-video nil))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background
                                       :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-background
                                   :inverse-video nil
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-background
                                       :inverse-video nil
                                       :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,sonokai-class (:inherit bold
                                :foreground ,sonokai-emphasis))
      (,sonokai-256-class (:inherit bold
                                    :foreground ,sonokai-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,sonokai-class (:background unspecified))
      (,sonokai-256-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,sonokai-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,sonokai-256-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,sonokai-class (:inherit italic :foreground ,sonokai-emphasis))
      (,sonokai-256-class (:inherit italic :foreground ,sonokai-256-emphasis))))

   `(font-latex-math-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(font-latex-sectioning-0-face
     ((,sonokai-class (:inherit font-latex-sectioning-1-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit font-latex-sectioning-1-face
                                    :height ,sonokai-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,sonokai-class (:inherit font-latex-sectioning-2-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit font-latex-sectioning-2-face
                                    :height ,sonokai-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,sonokai-class (:inherit font-latex-sectioning-3-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit font-latex-sectioning-3-face
                                    :height ,sonokai-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,sonokai-class (:inherit font-latex-sectioning-4-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit font-latex-sectioning-4-face
                                    :height ,sonokai-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,sonokai-class (:inherit font-latex-sectioning-5-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit font-latex-sectioning-5-face
                                    :height ,sonokai-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-yellow
                                :weight bold))
      (,sonokai-256-class (:inherit ,sonokai-pitch :
                                    foreground ,sonokai-256-yellow
                                    :weight bold))))

   `(font-latex-sedate-face
     ((,sonokai-class (:foreground ,sonokai-emphasis))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis))))

   `(font-latex-slide-title-face
     ((,sonokai-class (:inherit (,sonokai-pitch font-lock-type-face)
                                :weight bold
                                :height ,sonokai-height-plus-3))
      (,sonokai-256-class (:inherit (,sonokai-pitch font-lock-type-face)
                                    :weight bold
                                    :height ,sonokai-height-plus-3))))

   `(font-latex-string-face
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(font-latex-subscript-face
     ((,sonokai-class (:height ,sonokai-height-minus-1))
      (,sonokai-256-class (:height ,sonokai-height-minus-1))))

   `(font-latex-superscript-face
     ((,sonokai-class (:height ,sonokai-height-minus-1))
      (,sonokai-256-class (:height ,sonokai-height-minus-1))))

   `(font-latex-verbatim-face
     ((,sonokai-class (:inherit fixed-pitch
                                :foreground ,sonokai-foreground
                                :slant italic))
      (,sonokai-256-class (:inherit fixed-pitch
                                    :foreground ,sonokai-256-foreground
                                    :slant italic))))

   `(font-latex-warning-face
     ((,sonokai-class (:inherit bold
                                :foreground ,sonokai-orange))
      (,sonokai-256-class (:inherit bold
                                    :foreground ,sonokai-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-blue))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-blue))))

   `(ac-selection-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(ac-candidate-mouse-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(ac-completion-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :underline t))))

   `(ac-gtags-candidate-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-blue))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-blue))))

   `(ac-gtags-selection-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(ac-yasnippet-candidate-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-blue))))

   `(ahs-edit-mode-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-highlight))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-highlight))))

   `(ahs-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :background unspecified))))

   `(ahs-plugin-bod-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-violet ))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-green))))

   `(ahs-warning-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(android-mode-error-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :weight bold))))

   `(android-mode-info-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(android-mode-verbose-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(android-mode-warning-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :weight bold))))

   ;; bm
   `(bm-face
     ((,sonokai-class (:background ,sonokai-yellow-lc
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow-lc
                                       :foreground ,sonokai-256-background))))

   `(bm-fringe-face
     ((,sonokai-class (:background ,sonokai-yellow-lc
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow-lc
                                       :foreground ,sonokai-256-background))))

   `(bm-fringe-persistent-face
     ((,sonokai-class (:background ,sonokai-green-lc
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-green-lc
                                       :foreground ,sonokai-256-background))))

   `(bm-persistent-face
     ((,sonokai-class (:background ,sonokai-green-lc
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-green-lc
                                       :foreground ,sonokai-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(cfw:face-annotation
     ((,sonokai-class (:inherit cfw:face-day-title
                                :foreground ,sonokai-yellow))
      (,sonokai-256-class (:inherit cfw:face-day-title
                                    :foreground ,sonokai-256-yellow))))

   `(cfw:face-default-content
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(cfw:face-default-day
     ((,sonokai-class (:inherit cfw:face-day-title
                                :weight bold))
      (,sonokai-256-class (:inherit cfw:face-day-title
                                    :weight bold))))

   `(cfw:face-disable
     ((,sonokai-class (:inherit cfw:face-day-title
                                :foreground ,sonokai-comments))
      (,sonokai-256-class (:inherit cfw:face-day-title
                                    :foreground ,sonokai-256-comments))))

   `(cfw:face-grid
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(cfw:face-header
     ((,sonokai-class (:foreground ,sonokai-blue-hc
                                   :background ,sonokai-blue-lc
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-hc
                                       :background ,sonokai-256-blue-lc
                                       :weight bold))))

   `(cfw:face-holiday
     ((,sonokai-class (:background nil
                                   :foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:background nil
                                       :foreground ,sonokai-256-red
                                       :weight bold))))

   `(cfw:face-periods
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   `(cfw:face-select
     ((,sonokai-class (:background ,sonokai-magenta-lc
                                   :foreground ,sonokai-magenta-hc))
      (,sonokai-256-class (:background ,sonokai-256-magenta-lc
                                       :foreground ,sonokai-256-magenta-hc))))

   `(cfw:face-saturday
     ((,sonokai-class (:foreground ,sonokai-cyan-hc
                                   :background ,sonokai-cyan-lc))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan-hc
                                       :background ,sonokai-256-cyan-lc))))

   `(cfw:face-sunday
     ((,sonokai-class (:foreground ,sonokai-red-hc
                                   :background ,sonokai-red-lc
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red-hc
                                       :background ,sonokai-256-red-lc
                                       :weight bold))))

   `(cfw:face-title
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-yellow
                                :weight bold
                                :height ,sonokai-height-plus-4))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-yellow
                                    :weight bold
                                    :height ,sonokai-height-plus-4))))

   `(cfw:face-today
     ((,sonokai-class (:weight bold
                               :background ,sonokai-highlight-line
                               :foreground nil))
      (,sonokai-256-class (:weight bold
                                   :background ,sonokai-256-highlight-line
                                   :foreground nil))))

   `(cfw:face-today-title
     ((,sonokai-class (:background ,sonokai-yellow-lc
                                   :foreground ,sonokai-yellow-hc
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-yellow-lc
                                       :foreground ,sonokai-256-yellow-hc
                                       :weight bold))))

   `(cfw:face-toolbar
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,sonokai-class (:background ,sonokai-yellow-lc
                                   :foreground ,sonokai-yellow-hc
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-yellow-lc
                                       :foreground ,sonokai-256-yellow-hc
                                       :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,sonokai-class (:background ,sonokai-yellow-hc
                                   :foreground ,sonokai-yellow-lc
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-yellow-hc
                                       :foreground ,sonokai-256-yellow-lc
                                       :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background nil
                                   :box (:color ,sonokai-yellow :line-width (-1 . -1) :style nil)))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background nil
                                       :box (:color ,sonokai-256-yellow :line-width (-1 . -1) :style nil))) ))

   `(cider-enlightened-local
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(cider-instrumented-face
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :background nil
                                   :box (:color ,sonokai-violet :line-width (-1 . -1) :style nil)))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :background nil
                                       :box (:color ,sonokai-256-violet :line-width (-1 . -1) :style nil)))))

   `(cider-result-overlay-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background nil
                                   :box (:color ,sonokai-blue :line-width (-1 . -1) :style nil)))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background nil
                                       :box (:color ,sonokai-256-blue :line-width (-1 . -1) :style nil)))))

   `(cider-test-error-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-orange))))

   `(cider-test-failure-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-red))))

   `(cider-test-success-face
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-green))))

   `(cider-traced-face
     ((,sonokai-class :box (:color ,sonokai-blue :line-width (-1 . -1) :style nil))
      (,sonokai-256-class  :box (:color ,sonokai-256-blue :line-width (-1 . -1) :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold
                                       :underline t))))

   `(clojure-test-error-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold
                                       :underline t))))

   `(clojure-test-success-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold
                                       :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis))))

   `(company-tooltip-selection
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(company-tooltip-mouse
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(company-tooltip-common
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline t))))

   `(company-tooltip-common-selection
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-blue
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-blue
                                       :underline t))))

   `(company-preview
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis))))

   `(company-preview-common
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline t))))

   `(company-scrollbar-bg
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   `(company-scrollbar-fg
     ((,sonokai-class (:background ,sonokai-comments))
      (,sonokai-256-class (:background ,sonokai-256-comments))))

   `(company-tooltip-annotation
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-green))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-green))))

   `(company-template-field
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-blue))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :underline nil))))

   `(compilation-column-number
     ((,sonokai-class (:inherit font-lock-doc-face
                                :foreground ,sonokai-cyan
                                :underline nil))
      (,sonokai-256-class (:inherit font-lock-doc-face
                                    :foreground ,sonokai-256-cyan
                                    :underline nil))))

   `(compilation-enter-directory-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :underline nil))))

   `(compilation-error
     ((,sonokai-class (:inherit error
                                :underline nil))
      (,sonokai-256-class (:inherit error
                                    :underline nil))))

   `(compilation-error-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :underline nil))))

   `(compilation-face
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :underline nil))))

   `(compilation-info
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :underline nil
                                   :bold nil))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :underline nil
                                       :bold nil))))

   `(compilation-info-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline nil))))

   `(compilation-leave-directory-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :underline nil))))

   `(compilation-line-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :underline nil))))

   `(compilation-line-number
     ((,sonokai-class (:foreground ,sonokai-green
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :underline nil))))

   `(compilation-warning
     ((,sonokai-class (:inherit warning
                                :underline nil))
      (,sonokai-256-class (:inherit warning
                                    :underline nil))))

   `(compilation-warning-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal
                                       :underline nil))))

   `(compilation-mode-line-exit
     ((,sonokai-class (:inherit compilation-info
                                :foreground ,sonokai-green
                                :weight bold))
      (,sonokai-256-class (:inherit compilation-info
                                    :foreground ,sonokai-256-green
                                    :weight bold))))

   `(compilation-mode-line-fail
     ((,sonokai-class (:inherit compilation-error
                                :foreground ,sonokai-red
                                :weight bold))
      (,sonokai-256-class (:inherit compilation-error
                                    :foreground ,sonokai-256-red
                                    :weight bold))))

   `(compilation-mode-line-run
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(cscope-function-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(cscope-line-number-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(cscope-line-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(cscope-mouse-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :underline ,sonokai-emphasis
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :underline ,sonokai-256-emphasis
                                       :weight bold))))

   `(ctbl:face-continue-bar
     ((,sonokai-class (:background ,sonokai-gray
                                   :foreground ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-gray
                                       :foreground ,sonokai-256-yellow))))

   `(ctbl:face-row-select
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground
                                   :underline t))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground
                                       :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(coffee-mode-function-param
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :height ,sonokai-height-plus-3
                                :foreground ,sonokai-violet
                                :weight bold))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :height ,sonokai-height-plus-3
                                    :foreground ,sonokai-256-violet
                                    :weight bold))))

   `(custom-variable-tag
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-cyan
                                :height ,sonokai-height-plus-3))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-cyan
                                    :height ,sonokai-height-plus-3))))

   `(custom-comment-tag
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(custom-group-tag
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-blue
                                :height ,sonokai-height-plus-3))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-blue
                                    :height ,sonokai-height-plus-3))))

   `(custom-group-tag-1
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-red
                                :height ,sonokai-height-plus-3))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-red
                                    :height ,sonokai-height-plus-3))))

   `(custom-state
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   ;; diff
   `(diff-added
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background))))

   `(diff-changed
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background))))

   `(diff-removed
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background))))

   `(diff-header
     ((,sonokai-class (:background ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-background))))

   `(diff-file-header
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground
                                       :weight bold))))

   `(diff-refine-added
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-diff-green-base))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-diff-green-base))))

   `(diff-refine-change
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-diff-blue-base))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-diff-blue-base))))

   `(diff-refine-removed
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-diff-red-base))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-diff-red-base))))

   ;; diff-hl
   `(diff-hl-change
     ((,sonokai-class (:background ,sonokai-yellow-hc-alt
                                   :foreground ,sonokai-yellow-hc-alt))
      (,sonokai-256-class (:background ,sonokai-256-yellow-hc
                                       :foreground ,sonokai-256-yellow-hc))))

   `(diff-hl-delete
     ((,sonokai-class (:background ,sonokai-red-hc-alt
                                   :foreground ,sonokai-red-hc-alt))
      (,sonokai-256-class (:background ,sonokai-256-red-hc
                                       :foreground ,sonokai-256-red-hc))))

   `(diff-hl-insert
     ((,sonokai-class (:background ,sonokai-green-hc-alt
                                   :foreground ,sonokai-green-hc-alt))
      (,sonokai-256-class (:background ,sonokai-256-green-hc
                                       :foreground ,sonokai-256-green-hc))))

   `(diff-hl-unknown
     ((,sonokai-class (:background ,sonokai-violet-hc
                                   :foreground ,sonokai-violet-hc))
      (,sonokai-256-class (:background ,sonokai-256-violet-hc
                                       :foreground ,sonokai-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-red-emphasis))
      (,sonokai-256-class (:foreground: ,sonokai-256-foreground
                                        :background ,sonokai-256-diff-red-emphasis))))

   `(ediff-fine-diff-B
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-green-base))
      (,sonokai-256-class (:foreground: sonokai-256-foreground,
                                        :background ,sonokai-256-diff-green-base))))

   `(ediff-fine-diff-C
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-blue-base))
      (,sonokai-256-class (:foreground: sonokai-foreground,
                                        :background ,sonokai-256-diff-blue-base))))

   `(ediff-current-diff-A
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-red-alt))
      (,sonokai-256-class (:foreground ,sonokai-foreground
                                       :background ,sonokai-256-diff-red-alt))))

   `(ediff-current-diff-B
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-green-alt))
      (,sonokai-256-class (:foreground ,sonokai-foreground
                                       :background ,sonokai-256-diff-green-alt))))

   `(ediff-current-diff-C
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-diff-blue-alt))
      (,sonokai-256-class (:foreground ,sonokai-foreground
                                       :background ,sonokai-256-diff-blue-alt))))

   `(ediff-even-diff-A
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   `(ediff-odd-diff-A
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   `(ediff-even-diff-B
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   `(ediff-odd-diff-B
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   `(ediff-even-diff-C
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   `(ediff-odd-diff-C
     ((,sonokai-class (:background ,sonokai-background-alt
                                   :foreground ,sonokai-foreground ))
      (,sonokai-256-class (:background ,sonokai-256-background-alt
                                       :foreground ,sonokai-256-foreground ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) sonokai-class)
       (:underline (:style line :color ,sonokai-red)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-red-hc
                                   :background ,sonokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sonokai-256-class )
       (:underline (:style line :color ,sonokai-256-red)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-red-hc
                                       :background ,sonokai-256-red-lc
                                       :weight bold
                                       :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) sonokai-class)
       (:underline (:style line :color ,sonokai-yellow)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-yellow-hc
                                   :background ,sonokai-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sonokai-256-class )
       (:underline (:style line :color ,sonokai-256-yellow)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow-hc
                                       :background ,sonokai-256-yellow-lc
                                       :weight bold
                                       :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background unspecified
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background unspecified
                                       :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background unspecified
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background unspecified
                                       :weight bold))))

   `(edts-face-error-mode-line
     ((,sonokai-class (:background ,sonokai-red
                                   :foreground unspecified))
      (,sonokai-256-class (:background ,sonokai-256-red
                                       :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground unspecified))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(elfeed-search-feed-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(elfeed-search-tag-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(elfeed-search-title-face
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(elixir-atom-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))
   `(ein:cell-output-prompt
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))
   `(ein:notification-tab-normal
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))
   `(ein:notification-tab-selected
     ((,sonokai-class (:foreground ,sonokai-orange :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,sonokai-class (:inherit font-lock-string-face))
      (,sonokai-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,sonokai-class (:inherit font-lock-string-face))
      (,sonokai-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,sonokai-class (:inherit font-lock-string-face))
      (,sonokai-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,sonokai-class (:inherit font-lock-keyword-face))
      (,sonokai-256-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-red)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-red-hc
                                   :background ,sonokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-red)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-red-hc
                                       :background ,sonokai-256-red-lc
                                       :weight bold
                                       :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-orange)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-orange-hc
                                   :background ,sonokai-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-orange)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-orange-hc
                                       :background ,sonokai-256-orange-lc
                                       :weight bold
                                       :underline t))))

   ;; epc
   `(epc:face-title
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-background
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :underline nil))))

   ;; erc
   `(erc-action-face
     ((,sonokai-class (:inherit erc-default-face))
      (,sonokai-256-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,sonokai-class (:weight bold))
      (,sonokai-256-class (:weight bold))))

   `(erc-current-nick-face
     ((,sonokai-class (:foreground ,sonokai-blue :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight bold))))

   `(erc-dangerous-host-face
     ((,sonokai-class (:inherit font-lock-warning-face))
      (,sonokai-256-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(erc-highlight-face
     ((,sonokai-class (:inherit erc-default-face
                                :background ,sonokai-highlight))
      (,sonokai-256-class (:inherit erc-default-face
                                    :background ,sonokai-256-highlight))))

   `(erc-direct-msg-face
     ((,sonokai-class (:inherit erc-default-face))
      (,sonokai-256-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,sonokai-class (:inherit font-lock-warning-face))
      (,sonokai-256-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,sonokai-class (:inherit erc-default-face))
      (,sonokai-256-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(erc-keyword-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight bold))))

   `(erc-nick-default-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(erc-my-nick-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold))))

   `(erc-nick-msg-face
     ((,sonokai-class (:inherit erc-default-face))
      (,sonokai-256-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(erc-pal-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :weight bold))))

   `(erc-prompt-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :background ,sonokai-256-background
                                       :weight bold))))

   `(erc-timestamp-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :inherit bold))))

   `(eshell-ls-archive
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :inherit bold))))

   `(eshell-ls-backup
     ((,sonokai-class (:inherit font-lock-comment-face))
      (,sonokai-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,sonokai-class (:inherit font-lock-comment-face))
      (,sonokai-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :inherit bold))))

   `(eshell-ls-executable
     ((,sonokai-class (:foreground ,sonokai-green
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :inherit bold))))

   `(eshell-ls-unreadable
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(eshell-ls-missing
     ((,sonokai-class (:inherit font-lock-warning-face))
      (,sonokai-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,sonokai-class (:inherit font-lock-doc-face))
      (,sonokai-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :inherit bold))))

   `(eshell-ls-symlink
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-red-l
                                   :inherit italic))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-red-l
                                       :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-green-l
                                   :inherit italic))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line :foreground ,sonokai-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,sonokai-class (:inherit region))
      (,sonokai-256-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-orange
                                   :underline t
                                   :slant italic))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-orange
                                       :underline t
                                       :slant italic))))

   `(fic-face
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-orange
                                   :weight normal
                                   :slant italic))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-orange
                                       :weight normal
                                       :slant italic))))

   `(font-lock-fic-face
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-orange
                                   :weight normal
                                   :slant italic))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-orange
                                       :weight normal
                                       :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight normal
                                       :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-class (:foreground ,sonokai-red-hc
                                   :background ,sonokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-red-hc
                                       :background ,sonokai-256-red-lc
                                       :weight bold
                                       :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-class (:foreground ,sonokai-green-hc
                                   :background ,sonokai-green-lc))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-green-hc
                                       :background ,sonokai-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-class (:foreground ,sonokai-yellow-hc
                                   :background ,sonokai-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow-hc
                                       :background ,sonokai-256-yellow-lc
                                       :weight bold
                                       :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) sonokai-class)
       (:underline (:style line :color ,sonokai-red)))
      (,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sonokai-256-class )
       (:underline (:style line :color ,sonokai-256-red)))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :weight bold
                                       :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) sonokai-class)
       (:underline (:style line :color ,sonokai-orange)))
      (,sonokai-class (:foreground ,sonokai-orange
                                   :background ,sonokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sonokai-256-class )
       (:underline (:style line :color ,sonokai-256-orange)))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :background ,sonokai-256-background
                                       :weight bold
                                       :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) sonokai-class)
       (:underline (:style line :color ,sonokai-blue)))
      (,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sonokai-256-class )
       (:underline (:style line :color ,sonokai-256-blue)))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background
                                       :weight bold
                                       :underline t))))

   `(flycheck-fringe-error
     ((,sonokai-class (:foreground ,sonokai-red-l
                                   :background unspecified
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red-l
                                       :background unspecified
                                       :weight bold))))

   `(flycheck-fringe-warning
     ((,sonokai-class (:foreground ,sonokai-orange-l
                                   :background unspecified
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange-l
                                       :background unspecified
                                       :weight bold))))

   `(flycheck-fringe-info
     ((,sonokai-class (:foreground ,sonokai-blue-l
                                   :background unspecified
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-l
                                       :background unspecified
                                       :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-yellow)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-yellow)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold
                                       :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) sonokai-class)
       (:underline (:style wave :color ,sonokai-red)
                   :inherit unspecified))
      (,sonokai-class (:foreground ,sonokai-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sonokai-256-class )
       (:underline (:style wave :color ,sonokai-256-red)
                   :inherit unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold
                                       :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-green
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-green
                                    :weight bold))))

   `(git-gutter:deleted
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-red
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-red
                                    :weight bold))))

   `(git-gutter:modified
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-yellow
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-yellow
                                    :weight bold))))

   `(git-gutter:unchanged
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-highlight-line
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-highlight-line
                                    :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,sonokai-class (:foreground ,sonokai-green
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :inherit bold))))

   `(git-gutter-fr:deleted
     ((,sonokai-class (:foreground ,sonokai-red
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :inherit bold))))

   `(git-gutter-fr:modified
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-green
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-green
                                    :weight bold))))

   `(git-gutter+-deleted
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-red
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-red
                                    :weight bold))))

   `(git-gutter+-modified
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-yellow
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-yellow
                                    :weight bold))))

   `(git-gutter+-unchanged
     ((,sonokai-class (:inherit default
                                :background ,sonokai-background
                                :foreground ,sonokai-highlight-line
                                :weight bold))
      (,sonokai-256-class (:inherit default
                                    :background ,sonokai-256-background
                                    :foreground ,sonokai-256-highlight-line
                                    :weight bold))))

   `(git-gutter-fr+-added
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(git-gutter-fr+-deleted
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold))))

   `(git-gutter-fr+-modified
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-highlight-line
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-blue
                                       :background ,sonokai-256-highlight-line
                                       :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(guide-key/key-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(guide-key/prefix-command-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,sonokai-class (:inherit gnus-group-news-1-empty))
      (,sonokai-256-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,sonokai-class (:inherit gnus-group-news-2-empty))
      (,sonokai-256-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,sonokai-class (:inherit gnus-group-news-3-empty))
      (,sonokai-256-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,sonokai-class (:inherit gnus-group-news-low-empty))
      (,sonokai-256-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,sonokai-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,sonokai-256-class (:weight bold
                                   :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,sonokai-class (:inherit message-header-other))
      (,sonokai-256-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,sonokai-class (:inherit message-header-other))
      (,sonokai-256-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,sonokai-class (:inherit message-header-name))
      (,sonokai-256-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,sonokai-class (:inherit message-header-other))
      (,sonokai-256-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,sonokai-class (:inherit message-header-subject))
      (,sonokai-256-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(gnus-summary-high-ancient
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight bold))))

   `(gnus-summary-high-read
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(gnus-summary-high-ticked
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :weight bold))))

   `(gnus-summary-high-unread
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight bold))))

   `(gnus-summary-low-ancient
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-summary-low-read
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-summary-low-ticked
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(gnus-summary-low-unread
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-summary-normal-read
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-summary-normal-ticked
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(gnus-summary-normal-unread
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(gnus-summary-selected
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(gnus-cite-1
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-cite-2
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-cite-3
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-cite-4
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-cite-5
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-cite-6
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-cite-7
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(gnus-cite-8
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(gnus-cite-9
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(gnus-cite-10
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(gnus-cite-11
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(gnus-group-news-1-empty
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(gnus-group-news-2-empty
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-group-news-3-empty
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(gnus-group-news-4-empty
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-group-news-5-empty
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(gnus-group-news-6-empty
     ((,sonokai-class (:foreground ,sonokai-blue-lc))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(gnus-signature
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(gnus-x-face
     ((,sonokai-class (:background ,sonokai-foreground
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-foreground
                                       :foreground ,sonokai-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(helm-apt-installed
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-bookmark-directory
     ((,sonokai-class (:inherit helm-ff-directory))
      (,sonokai-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(helm-bookmark-gnus
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(helm-bookmark-info
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-bookmark-man
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(helm-bookmark-w3m
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(helm-bookmarks-su
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(helm-buffer-file
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(helm-buffer-directory
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(helm-buffer-process
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(helm-buffer-saved-out
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :inverse-video t))))

   `(helm-buffer-size
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(helm-candidate-number
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :bold t))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :bold t))))

   `(helm-ff-directory
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(helm-ff-executable
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-ff-file
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-orange
                                   :slant italic))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-orange
                                       :slant italic))))

   `(helm-ff-prefix
     ((,sonokai-class (:background ,sonokai-green
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-green
                                       :foreground ,sonokai-256-background))))

   `(helm-ff-symlink
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(helm-grep-file
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :underline t))))

   `(helm-grep-finish
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-grep-lineno
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(helm-grep-match
     ((,sonokai-class (:inherit helm-match))
      (,sonokai-256-class (:inherit helm-match))))

   `(helm-grep-running
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(helm-header
     ((,sonokai-class (:inherit header-line))
      (,sonokai-256-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(helm-lisp-show-completion
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-highlight-line
                                   :bold t))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-highlight-line
                                       :bold t))))

   `(helm-M-x-key
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :underline t))))

   `(helm-moccur-buffer
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :underline t))))

   `(helm-match
     ((,sonokai-class (:foreground ,sonokai-green :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green :inherit bold))))

   `(helm-match-item
     ((,sonokai-class (:inherit helm-match))
      (,sonokai-256-class (:inherit helm-match))))

   `(helm-selection
     ((,sonokai-class (:background ,sonokai-highlight
                                   :inherit bold
                                   :underline nil))
      (,sonokai-256-class (:background ,sonokai-256-highlight
                                       :inherit bold
                                       :underline nil))))

   `(helm-selection-line
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :underline nil))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :underline nil))))

   `(helm-separator
     ((,sonokai-class (:foreground ,sonokai-gray))
      (,sonokai-256-class (:foreground ,sonokai-256-gray))))

   `(helm-source-header
     ((,sonokai-class (:background ,sonokai-violet-l
                                   :foreground ,sonokai-background
                                   :underline nil))
      (,sonokai-256-class (:background ,sonokai-256-violet-l
                                       :foreground ,sonokai-256-background
                                       :underline nil))))

   `(helm-swoop-target-line-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-time-zone-current
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(helm-time-zone-home
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(helm-visible-mark
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-magenta :bold t))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,sonokai-class :foreground ,sonokai-blue)
      (,sonokai-256-class  :foreground ,sonokai-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,sonokai-class :foreground ,sonokai-blue-l)
      (,sonokai-256-class  :foreground ,sonokai-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,sonokai-class :foreground ,sonokai-blue-l)
      (,sonokai-256-class  :foreground ,sonokai-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,sonokai-class :foreground ,sonokai-orange)
      (,sonokai-256-class  :foreground ,sonokai-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,sonokai-class :foreground ,sonokai-green)
      (,sonokai-256-class  :foreground ,sonokai-256-green)))

   `(helm-ls-git-added-modified-face
     ((,sonokai-class :foreground ,sonokai-green-l)
      (,sonokai-256-class  :foreground ,sonokai-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,sonokai-class :foreground ,sonokai-red)
      (,sonokai-256-class  :foreground ,sonokai-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,sonokai-class :foreground ,sonokai-red-l)
      (,sonokai-256-class  :foreground ,sonokai-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,sonokai-class :foreground ,sonokai-yellow)
      (,sonokai-256-class  :foreground ,sonokai-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,sonokai-class (:foreground ,sonokai-yellow-lc
                                   :background ,sonokai-yellow-hc))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow-lc
                                       :background ,sonokai-256-yellow-hc))))

   `(hi-pink
     ((,sonokai-class (:foreground ,sonokai-magenta-lc
                                   :background ,sonokai-magenta-hc))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta-lc
                                       :background ,sonokai-256-magenta-hc))))

   `(hi-green
     ((,sonokai-class (:foreground ,sonokai-green-lc
                                   :background ,sonokai-green-hc))
      (,sonokai-256-class (:foreground ,sonokai-256-green-lc
                                       :background ,sonokai-256-green-hc))))

   `(hi-blue
     ((,sonokai-class (:foreground ,sonokai-blue-lc
                                   :background ,sonokai-blue-hc))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-lc
                                       :background ,sonokai-256-blue-hc))))

   `(hi-black-b
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background
                                       :weight bold))))

   `(hi-blue-b
     ((,sonokai-class (:foreground ,sonokai-blue-lc
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-lc
                                       :weight bold))))

   `(hi-green-b
     ((,sonokai-class (:foreground ,sonokai-green-lc
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green-lc
                                       :weight bold))))

   `(hi-red-b
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold))))

   `(hi-black-hb
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background
                                       :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(highlight-changes-delete
     ((,sonokai-class (:foreground ,sonokai-red
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   `(highlight-indentation-current-column-face
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,sonokai-class (:background ,sonokai-highlight))
      (,sonokai-256-class (:background ,sonokai-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(hl-line-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal))))

   `(ido-only-match
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-yellow
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-yellow
                                       :weight normal))))

   `(ido-subdir
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(ido-incomplete-regexp
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold ))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold ))))

   `(ido-indicator
     ((,sonokai-class (:background ,sonokai-red
                                   :foreground ,sonokai-background
                                   :width condensed))
      (,sonokai-256-class (:background ,sonokai-256-red
                                       :foreground ,sonokai-256-background
                                       :width condensed))))

   `(ido-virtual
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   ;; info
   `(info-header-xref
     ((,sonokai-class (:foreground ,sonokai-green
                                   :inherit bold
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :inherit bold
                                       :underline t))))

   `(info-menu
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(info-node
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :inherit bold))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :inherit bold))))

   `(info-quoted-name
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(info-reference-item
     ((,sonokai-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,sonokai-256-class (:background nil
                                       :underline t
                                       :inherit bold))))

   `(info-string
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(info-title-1
     ((,sonokai-class (:height ,sonokai-height-plus-4))
      (,sonokai-256-class (:height ,sonokai-height-plus-4))))

   `(info-title-2
     ((,sonokai-class (:height ,sonokai-height-plus-3))
      (,sonokai-256-class (:height ,sonokai-height-plus-3))))

   `(info-title-3
     ((,sonokai-class (:height ,sonokai-height-plus-2))
      (,sonokai-256-class (:height ,sonokai-height-plus-2))))

   `(info-title-4
     ((,sonokai-class (:height ,sonokai-height-plus-1))
      (,sonokai-256-class (:height ,sonokai-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,sonokai-class (:background ,sonokai-gray :inherit bold))
      (,sonokai-256-class (:background ,sonokai-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,sonokai-class (:inherit bold))
      (,sonokai-256-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,sonokai-class (:foreground ,sonokai-green
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :underline t))))

   `(ivy-remote
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(swiper-line-face
     ((,sonokai-class (:background ,sonokai-highlight-line))))

   `(swiper-match-face-1
     ((,sonokai-class (:background ,sonokai-gray-d))))

   `(swiper-match-face-2
     ((,sonokai-class (:background ,sonokai-green))))

   `(swiper-match-face-3
     ((,sonokai-class (:background ,sonokai-orange))))

   `(swiper-match-face-4
     ((,sonokai-class (:background ,sonokai-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-red))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-red))))

   `(jabber-activity-personal-face
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-blue))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-blue))))

   `(jabber-chat-error
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-red))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-red))))

   `(jabber-chat-prompt-foreign
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-red))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-red))))

   `(jabber-chat-prompt-local
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-blue))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-blue))))

   `(jabber-chat-prompt-system
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-green))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-green))))

   `(jabber-chat-text-foreign
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(jabber-chat-text-local
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,sonokai-class (:underline t
                                  :foreground ,sonokai-green))
      (,sonokai-256-class (:underline t
                                      :foreground ,sonokai-256-green))))

   `(jabber-roster-user-away
     ((,sonokai-class (:slant italic
                              :foreground ,sonokai-green))
      (,sonokai-256-class (:slant italic
                                  :foreground ,sonokai-256-green))))

   `(jabber-roster-user-chatty
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-orange))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-orange))))

   `(jabber-roster-user-dnd
     ((,sonokai-class (:slant italic
                              :foreground ,sonokai-red))
      (,sonokai-256-class (:slant italic
                                  :foreground ,sonokai-256-red))))

   `(jabber-roster-user-error
     ((,sonokai-class (:weight light
                               :slant italic
                               :foreground ,sonokai-red))
      (,sonokai-256-class (:weight light
                                   :slant italic
                                   :foreground ,sonokai-256-red))))

   `(jabber-roster-user-offline
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(jabber-roster-user-online
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-blue))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-blue))))

   `(jabber-roster-user-xa
     ((,sonokai-class (:slant italic
                              :foreground ,sonokai-magenta))
      (,sonokai-256-class (:slant italic
                                  :foreground ,sonokai-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(js2-external-variable
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(js2-function-call
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(js2-function-param
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(js2-instance-member
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(js2-jsdoc-tag
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(js2-jsdoc-type
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(js2-jsdoc-value
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(js2-magic-paren
     ((,sonokai-class (:underline t))
      (,sonokai-256-class (:underline t))))

   `(js2-object-property
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(js2-private-function-call
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(js2-private-member
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(js2-warning
     ((,sonokai-class (:underline ,sonokai-orange))
      (,sonokai-256-class (:underline ,sonokai-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,sonokai-class (:inherit bold))
      (,sonokai-256-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,sonokai-class (:inherit default
                                :foreground ,sonokai-line-number
                                :background ,sonokai-fringe-bg
                                :underline nil))
      (,sonokai-256-class (:inherit default
                                    :foreground ,sonokai-256-line-number
                                    :background ,sonokai-256-fringe-bg
                                    :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,sonokai-class (:foreground ,sonokai-line-number
                                   :background ,sonokai-fringe-bg
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-line-number
                                       :background ,sonokai-256-fringe-bg
                                       :underline nil))))

   `(line-number-current-line
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :background ,sonokai-fringe-bg
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :background ,sonokai-256-fringe-bg
                                       :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,sonokai-class (:inherit default
                                :foreground ,sonokai-orange
                                :background ,sonokai-fringe-bg
                                :underline nil))
      (,sonokai-256-class (:inherit default
                                    :foreground ,sonokai-256-orange
                                    :background ,sonokai-256-fringe-bg
                                    :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header
     ((,sonokai-class (:inherit org-document-title))
      (,sonokai-256-class (:inherit org-document-title))))

   `(lsp-ui-doc-background
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-highlight-line))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,sonokai-class (:inherit disonokai-red-directory))
      (,sonokai-256-class (:inherit disonokai-red-directory))))

   `(lusty-file-face
     ((,sonokai-class nil)
      (,sonokai-256-class  nil)))

   `(lusty-match-face
     ((,sonokai-class (:inherit ido-first-match))
      (,sonokai-256-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :weight bold))))

   ;; magit
   `(magit-bisect-good
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-bisect-skip
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(magit-bisect-bad
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(magit-blame-highlight
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-highlight-alt))))

   `(magit-diff-file-heading-selection
     ((,sonokai-class (:inherit magit-diff-file-heading-highlight
                                :foreground ,sonokai-orange-d))
      (,sonokai-256-class (:inherit magit-diff-file-heading-highlight
                                    :foreground ,sonokai-256-orange-d))))

   `(magit-diff-hunk-heading
     ((,sonokai-class (:foreground ,sonokai-gray-d
                                   :background ,sonokai-gray-l))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-d
                                       :background ,sonokai-256-gray-l))))

   `(magit-diff-hunk-heading-highlight
     ((,sonokai-class (:foreground ,sonokai-background
                                   :background ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-background
                                       :background ,sonokai-256-foreground))))

   `(magit-diff-hunk-heading-selection
     ((,sonokai-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,sonokai-orange))
      (,sonokai-256-class (:inherit magit-diff-hunk-heading-highlight
                                    :foreground ,sonokai-256-orange))))

   `(magit-diff-lines-heading
     ((,sonokai-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,sonokai-background
                                :background ,sonokai-orange-l))
      (,sonokai-256-class (:inherit magit-diff-hunk-heading-highlight
                                    :foreground ,sonokai-256-background
                                    :background ,sonokai-256-orange-l))))

   `(magit-diff-added
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background))))

   `(magit-diff-removed
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background))))

   `(magit-diff-base
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-background))))

   `(magit-diff-context
     ((,sonokai-class (:foreground ,sonokai-gray-l))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-l))))

   `(magit-diff-added-highlight
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-highlight-alt))))

   `(magit-diff-removed-highlight
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-highlight-alt))))

   `(magit-diff-base-highlight
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-highlight-alt))))

   `(magit-diff-context-highlight
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-highlight-alt))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-highlight-alt))))

   `(magit-diffstat-added
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-diffstat-removed
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(magit-log-graph
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(magit-log-author
     ((,sonokai-class (:foreground ,sonokai-red-d
                                   :slant normal
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-red-d
                                       :slant normal
                                       :weight normal))))

   `(magit-log-date
     ((,sonokai-class (:foreground ,sonokai-gray
                                   :slant normal
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-gray
                                       :slant normal
                                       :weight normal))))

   `(magit-process-ok
     ((,sonokai-class (:inherit magit-section-heading
                                :foreground ,sonokai-green))
      (,sonokai-256-class (:inherit magit-section-heading
                                    :foreground ,sonokai-256-green))))

   `(magit-process-ng
     ((,sonokai-class (:inherit magit-section-heading
                                :foreground ,sonokai-red))
      (,sonokai-256-class (:inherit magit-section-heading
                                    :foreground ,sonokai-256-red))))

   `(magit-reflog-commit
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-reflog-amend
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   `(magit-reflog-merge
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-reflog-checkout
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(magit-reflog-reset
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(magit-reflog-rebase
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(magit-reflog-cherry-pick
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-reflog-remote
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(magit-reflog-other
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(magit-section-highlight
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(magit-section-heading
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(magit-section-heading-selection
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(magit-sequence-stop
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(magit-sequence-part
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(magit-sequence-head
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(magit-sequence-drop
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(magit-dimmed
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(magit-hash
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(magit-tag
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(magit-branch-remote
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(magit-branch-local
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(magit-refname
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(magit-signature-good
     ((,sonokai-class (:foreground ,sonokai-green-d))
      (,sonokai-256-class (:foreground ,sonokai-256-green-d))))

   `(magit-signature-bad
     ((,sonokai-class (:foreground ,sonokai-red-d
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red-d
                                       :weight bold))))

   `(magit-signature-untrusted
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(magit-signature-expired
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(magit-signature-revoked
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   `(magit-signature-error
     ((,sonokai-class (:foreground ,sonokai-red-l))
      (,sonokai-256-class (:foreground ,sonokai-256-red-l))))

   `(magit-cherry-unmatched
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(magit-cherry-equivalent
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   ;; man
   `(Man-overstrike
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight bold))))

   `(Man-reverse
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(Man-underline
     ((,sonokai-class (:foreground ,sonokai-green :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(monky-diff-add
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(monky-diff-del
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(markdown-header-face-1
     ((,sonokai-class (:inherit markdown-header-face
                                :height ,sonokai-height-plus-4))
      (,sonokai-256-class (:inherit markdown-header-face
                                    :height ,sonokai-height-plus-4))))

   `(markdown-header-face-2
     ((,sonokai-class (:inherit markdown-header-face
                                :height ,sonokai-height-plus-3))
      (,sonokai-256-class (:inherit markdown-header-face
                                    :height ,sonokai-height-plus-3))))

   `(markdown-header-face-3
     ((,sonokai-class (:inherit markdown-header-face
                                :height ,sonokai-height-plus-2))
      (,sonokai-256-class (:inherit markdown-header-face
                                    :height ,sonokai-height-plus-2))))

   `(markdown-header-face-4
     ((,sonokai-class (:inherit markdown-header-face
                                :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:inherit markdown-header-face
                                    :height ,sonokai-height-plus-1))))

   `(markdown-header-face-5
     ((,sonokai-class (:inherit markdown-header-face))
      (,sonokai-256-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,sonokai-class (:inherit markdown-header-face))
      (,sonokai-256-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(message-header-name
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(message-header-other
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight normal))))

   `(message-header-to
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight normal))))

   `(message-header-cc
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight normal))))

   `(message-header-newsgroups
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(message-header-subject
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :weight normal))))

   `(message-header-xheader
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(message-mml
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(message-separator
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(mew-face-header-from
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(mew-face-header-date
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-header-to
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-header-key
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-header-private
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-header-important
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(mew-face-header-marginal
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight bold))))

   `(mew-face-header-warning
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-header-xmew
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-header-xmew-bad
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-body-url
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(mew-face-body-comment
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :slant italic))))

   `(mew-face-body-cite1
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-body-cite2
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(mew-face-body-cite3
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(mew-face-body-cite4
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(mew-face-body-cite5
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-mark-review
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(mew-face-mark-escape
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-mark-delete
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-mark-unlink
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(mew-face-mark-refile
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-mark-unread
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(mew-face-eof-message
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(mew-face-eof-part
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(mingus-pausing-face
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   `(mingus-playing-face
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(mingus-playlist-face
     ((,sonokai-class (:foreground ,sonokai-cyan ))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan ))))

   `(mingus-song-file-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(mingus-stopped-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,sonokai-class (:background ,sonokai-violet-d))
      (,sonokai-256-class (:background ,sonokai-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,sonokai-class (:background ,sonokai-orange-d))
      (,sonokai-256-class (:background ,sonokai-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,sonokai-class (:background ,sonokai-cyan-d))
      (,sonokai-256-class (:background ,sonokai-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,sonokai-class (:background ,sonokai-blue-d))
      (,sonokai-256-class (:background ,sonokai-256-blue-d))))

   `(mmm-output-submode-face
     ((,sonokai-class (:background ,sonokai-red-d))
      (,sonokai-256-class (:background ,sonokai-256-red-d))))

   `(mmm-special-submode-face
     ((,sonokai-class (:background ,sonokai-green-d))
      (,sonokai-256-class (:background ,sonokai-256-green-d))))

   `(mmm-code-submode-face
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   `(mmm-default-submode-face
     ((,sonokai-class (:background ,sonokai-gray-d))
      (,sonokai-256-class (:background ,sonokai-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,sonokai-class (:underline t))
      (,sonokai-256-class (:underline t))))

   `(moccur-edit-done-face
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background
                                       :slant italic))))

   `(moccur-edit-face
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-background))))

   `(moccur-edit-file-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(moccur-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :weight bold))))

   `(search-buffers-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :weight bold))))

   `(search-buffers-header-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-yellow
                                       :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-2-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-3-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-4-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-5-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-6-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :slant italic
                                       :weight normal))))

   `(mu4e-cited-7-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :slant italic
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :slant italic
                                       :weight normal))))

   `(mu4e-flagged-face
     ((,sonokai-class (:foreground ,sonokai-magenta
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :weight bold))))

   `(mu4e-view-url-number-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal))))

   `(mu4e-warning-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :slant normal
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :slant normal
                                       :weight bold))))

   `(mu4e-header-highlight-face
     ((,sonokai-class (:inherit unspecified
                                :foreground unspecified
                                :background ,sonokai-highlight-line
                                :underline ,sonokai-emphasis
                                :weight normal))
      (,sonokai-256-class (:inherit unspecified
                                    :foreground unspecified
                                    :background ,sonokai-256-highlight-line
                                    :underline ,sonokai-256-emphasis
                                    :weight normal))))


   `(mu4e-draft-face
     ((,sonokai-class (:inherit font-lock-string-face))
      (,sonokai-256-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,sonokai-class (:inherit font-lock-comment-face))
      (,sonokai-256-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,sonokai-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,sonokai-256-class (:inherit font-lock-builtin-face
                                    :weight normal))))

   `(mu4e-header-face
     ((,sonokai-class (:inherit default))
      (,sonokai-256-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,sonokai-class (:inherit font-lock-preprocessor-face))
      (,sonokai-256-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,sonokai-class (:inherit font-lock-type-face))
      (,sonokai-256-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,sonokai-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,sonokai-256-class (:inherit font-lock-pseudo-keyword-face
                                    :weight bold))))

   `(mu4e-moved-face
     ((,sonokai-class (:inherit font-lock-comment-face
                                :slant italic))
      (,sonokai-256-class (:inherit font-lock-comment-face
                                    :slant italic))))

   `(mu4e-ok-face
     ((,sonokai-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,sonokai-256-class (:inherit font-lock-comment-face
                                    :slant normal
                                    :weight bold))))

   `(mu4e-replied-face
     ((,sonokai-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,sonokai-256-class (:inherit font-lock-builtin-face
                                    :weight normal))))

   `(mu4e-system-face
     ((,sonokai-class (:inherit font-lock-comment-face
                                :slant italic))
      (,sonokai-256-class (:inherit font-lock-comment-face
                                    :slant italic))))

   `(mu4e-title-face
     ((,sonokai-class (:inherit font-lock-type-face
                                :weight bold))
      (,sonokai-256-class (:inherit font-lock-type-face
                                    :weight bold))))

   `(mu4e-trashed-face
     ((,sonokai-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,sonokai-256-class (:inherit font-lock-comment-face
                                    :strike-through t))))

   `(mu4e-unread-face
     ((,sonokai-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,sonokai-256-class (:inherit font-lock-keyword-face
                                    :weight bold))))

   `(mu4e-view-attach-number-face
     ((,sonokai-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,sonokai-256-class (:inherit font-lock-variable-name-face
                                    :weight bold))))

   `(mu4e-view-contact-face
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :weight normal))))

   `(mu4e-view-header-key-face
     ((,sonokai-class (:inherit message-header-name
                                :weight normal))
      (,sonokai-256-class (:inherit message-header-name
                                    :weight normal))))

   `(mu4e-view-header-value-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :weight normal
                                   :slant normal))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :weight normal
                                       :slant normal))))

   `(mu4e-view-link-face
     ((,sonokai-class (:inherit link))
      (,sonokai-256-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight normal
                                       :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(nav-face-button-num
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(nav-face-dir
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(nav-face-hdir
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(nav-face-file
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(nav-face-hfile
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background
                                       :weight bold))))


   `(neo-header-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background))))

   `(neo-root-dir-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background))))

   `(neo-dir-link-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-background))))

   `(neo-file-link-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(neo-button-face
     ((,sonokai-class (:underline nil))
      (,sonokai-256-class (:underline nil))))

   `(neo-expand-btn-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(neo-vc-default-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(neo-vc-user-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :slant italic))))

   `(neo-vc-up-to-date-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(neo-vc-edited-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(neo-vc-needs-update-face
     ((,sonokai-class (:underline t))
      (,sonokai-256-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-comments))))

   `(neo-vc-added-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(neo-vc-removed-face
     ((,sonokai-class (:strike-through t))
      (,sonokai-256-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(neo-vc-missing-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(neo-vc-ignored-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,sonokai-class (:foreground ,sonokai-gray-l))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-l))))

   `(markup-table-face
     ((,sonokai-class (:foreground ,sonokai-blue-hc
                                   :background ,sonokai-blue-lc))
      (,sonokai-256-class (:foreground ,sonokai-256-blue-hc
                                       :background ,sonokai-256-blue-lc))))

   `(markup-verbatim-face
     ((,sonokai-class (:background ,sonokai-orange-lc))
      (,sonokai-256-class (:background ,sonokai-256-orange-lc))))

   `(markup-list-face
     ((,sonokai-class (:foreground ,sonokai-violet-hc
                                   :background ,sonokai-violet-lc))
      (,sonokai-256-class (:foreground ,sonokai-256-violet-hc
                                       :background ,sonokai-256-violet-lc))))

   `(markup-replacement-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(markup-complex-replacement-face
     ((,sonokai-class (:foreground ,sonokai-violet-hc
                                   :background ,sonokai-violet-lc))
      (,sonokai-256-class (:foreground ,sonokai-256-violet-hc
                                       :background ,sonokai-256-violet-lc))))

   `(markup-gen-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(markup-secondary-text-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,sonokai-height-plus-1
                                   :underline nil
                                   :box (:line-width (2 . 2) :color ,sonokai-background)))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-highlight-line
                                       :weight bold
                                       :slant normal
                                       :inverse-video nil
                                       :height ,sonokai-height-plus-1
                                       :underline nil
                                       :box (:line-width (2 . 2) :color ,sonokai-256-background)))))

   `(org-agenda-calendar-event
     ((,sonokai-class (:foreground ,sonokai-emphasis))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :slant italic))))

   `(org-agenda-date
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width (2 . 2) :color ,sonokai-background)))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video nil
                                       :overline nil
                                       :slant normal
                                       :height 1.0
                                       :box (:line-width (2 . 2) :color ,sonokai-256-background)))) t)

   `(org-agenda-date-weekend
     ((,sonokai-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,sonokai-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,sonokai-256-class (:inherit org-agenda-date
                                    :inverse-video nil
                                    :background unspecified
                                    :foreground ,sonokai-256-comments
                                    :weight unspecified
                                    :underline t
                                    :overline nil
                                    :box unspecified))) t)

   `(org-agenda-date-today
     ((,sonokai-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,sonokai-blue
                                :background ,sonokai-background))
      (,sonokai-256-class (:inherit org-agenda-date
                                    :inverse-video t
                                    :weight bold
                                    :underline unspecified
                                    :overline nil
                                    :box unspecified
                                    :foreground ,sonokai-256-blue
                                    :background ,sonokai-256-background))) t)

   `(org-agenda-done
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :slant italic))) t)

   `(org-archived
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :weight normal))))

   `(org-block
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background-d))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background-d))))

   `(org-block-background
     ((,sonokai-class (:background ,sonokai-background-d))
      (,sonokai-256-class (:background ,sonokai-256-background-d))))

   `(org-block-begin-line
     ((,sonokai-class (:foreground ,sonokai-gray-l
                                   :weight bold
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-l
                                       :weight bold
                                       :slant italic))))

   `(org-block-end-line
     ((,sonokai-class (:foreground ,sonokai-gray-l
                                   :weight bold
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-l
                                       :weight bold
                                       :slant italic))))

   `(org-checkbox
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground
                                   :box nil))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground
                                       :box nil))))

   `(org-code
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background-d))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background-d))))

   `(org-date
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline t))))

   `(org-done
     ((,sonokai-class (:weight bold
                               :foreground ,sonokai-green))
      (,sonokai-256-class (:weight bold
                                   :foreground ,sonokai-256-green))))

   `(org-ellipsis
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(org-formula
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(org-headline-done
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(org-hide
     ((,sonokai-class (:foreground ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-background))))

   `(org-level-1
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :height ,sonokai-height-plus-4
                                :foreground ,sonokai-orange))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :height ,sonokai-height-plus-4
                                    :foreground ,sonokai-256-orange))))

   `(org-level-2
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :height ,sonokai-height-plus-3
                                :foreground ,sonokai-green))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :height ,sonokai-height-plus-3
                                    :foreground ,sonokai-256-green))))

   `(org-level-3
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :height ,sonokai-height-plus-2
                                :foreground ,sonokai-blue))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :height ,sonokai-height-plus-2
                                    :foreground ,sonokai-256-blue))))

   `(org-level-4
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :height ,sonokai-height-plus-1
                                :foreground ,sonokai-yellow))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :height ,sonokai-height-plus-1
                                    :foreground ,sonokai-256-yellow))))

   `(org-level-5
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-cyan))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-cyan))))

   `(org-level-6
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-green))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-green))))

   `(org-level-7
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-red))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-red))))

   `(org-level-8
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-blue))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-blue))))

   `(org-link
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :underline t))))

   `(org-sexp-date
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(org-scheduled
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(org-scheduled-previously
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(org-scheduled-today
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :weight normal))))

   `(org-special-keyword
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :weight bold))))

   `(org-table
     ((,sonokai-class (:foreground ,sonokai-white-d))
      (,sonokai-256-class (:foreground ,sonokai-256-white-d))))

   `(org-tag
     ((,sonokai-class (:weight bold))
      (,sonokai-256-class (:weight bold))))

   `(org-time-grid
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(org-todo
     ((,sonokai-class (:foreground ,sonokai-red-d
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red-d
                                       :weight bold))))

   `(org-upcoming-deadline
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal
                                       :underline nil))))

   `(org-warning
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :weight normal
                                   :underline nil))
      (,sonokai-256-class (:foreground ,sonokai-256-orange-d
                                       :weight normal
                                       :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,sonokai-class (:background ,sonokai-blue-lc
                                   :foreground ,sonokai-blue-hc))
      (,sonokai-256-class (:background ,sonokai-256-blue-lc
                                       :foreground ,sonokai-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,sonokai-class (:background ,sonokai-blue-lc))
      (,sonokai-256-class (:background ,sonokai-256-blue-lc))))

   `(org-habit-ready-face
     ((,sonokai-class (:background ,sonokai-green-lc
                                   :foreground ,sonokai-green))
      (,sonokai-256-class (:background ,sonokai-256-green-lc
                                       :foreground ,sonokai-256-green))))

   `(org-habit-ready-future-face
     ((,sonokai-class (:background ,sonokai-green-lc))
      (,sonokai-256-class (:background ,sonokai-256-green-lc))))

   `(org-habit-alert-face
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-yellow-lc))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,sonokai-class (:background ,sonokai-yellow-lc))
      (,sonokai-256-class (:background ,sonokai-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,sonokai-class (:background ,sonokai-red
                                   :foreground ,sonokai-red-lc))
      (,sonokai-256-class (:background ,sonokai-256-red
                                       :foreground ,sonokai-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,sonokai-class (:background ,sonokai-red-lc))
      (,sonokai-256-class (:background ,sonokai-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(org-agenda-restriction-lock
     ((,sonokai-class (:background ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-yellow))))

   `(org-clock-overlay
     ((,sonokai-class (:background ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-yellow))))

   `(org-column
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :strike-through nil
                                       :underline nil
                                       :slant normal
                                       :weight normal
                                       :inherit default))))

   `(org-column-title
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :underline t
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :underline t
                                       :weight bold))))

   `(org-date-selected
     ((,sonokai-class (:foreground ,sonokai-red
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :inverse-video t))))

   `(org-document-info
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(org-document-title
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :weight bold
                                   :height ,sonokai-height-plus-4))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :weight bold
                                       :height ,sonokai-height-plus-4))))

   `(org-drawer
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(org-footnote
     ((,sonokai-class (:foreground ,sonokai-magenta
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :underline t))))

   `(org-latex-and-export-specials
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(org-mode-line-clock-overrun
     ((,sonokai-class (:inherit mode-line))
      (,sonokai-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,sonokai-class (:inherit org-level-1))
      (,sonokai-256-class (:inherit org-level-1))))

   `(outline-2
     ((,sonokai-class (:inherit org-level-2))
      (,sonokai-256-class (:inherit org-level-2))))

   `(outline-3
     ((,sonokai-class (:inherit org-level-3))
      (,sonokai-256-class (:inherit org-level-3))))

   `(outline-4
     ((,sonokai-class (:inherit org-level-4))
      (,sonokai-256-class (:inherit org-level-4))))

   `(outline-5
     ((,sonokai-class (:inherit org-level-5))
      (,sonokai-256-class (:inherit org-level-5))))

   `(outline-6
     ((,sonokai-class (:inherit org-level-6))
      (,sonokai-256-class (:inherit org-level-6))))

   `(outline-7
     ((,sonokai-class (:inherit org-level-7))
      (,sonokai-256-class (:inherit org-level-7))))

   `(outline-8
     ((,sonokai-class (:inherit org-level-8))
      (,sonokai-256-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,sonokai-256-class (:foreground ,sonokai-comments))))

   ;; perspective
   `(persp-selected-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight normal))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight normal))))

   ;; popup
   `(popup-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   `(popup-isearch-match
     ((,sonokai-class (:background ,sonokai-green))
      (,sonokai-256-class (:background ,sonokai-256-green))))

   `(popup-menu-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   `(popup-menu-mouse-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-foreground))))

   `(popup-menu-selection-face
     ((,sonokai-class (:background ,sonokai-magenta
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-magenta
                                       :foreground ,sonokai-256-background))))

   `(popup-scroll-bar-background-face
     ((,sonokai-class (:background ,sonokai-comments))
      (,sonokai-256-class (:background ,sonokai-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,sonokai-class (:background ,sonokai-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-emphasis))))

   `(popup-tip-face
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-background
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-background
                                       :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,sonokai-class (:foreground ,sonokai-green-d))
      (,sonokai-256-class (:foreground ,sonokai-256-green-d))))

   `(realgud-overlay-arrow2
     ((,sonokai-class (:foreground ,sonokai-yellow-d))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,sonokai-class (:foreground ,sonokai-orange-d))
      (,sonokai-256-class (:foreground ,sonokai-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,sonokai-class (:inherit error))
      (,sonokai-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,sonokai-class (:inherit secondary-selection))
      (,sonokai-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,sonokai-class (:foreground ,sonokai-red-d))
      (,sonokai-256-class (:foreground ,sonokai-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,sonokai-class (:inherit secondary-selection))
      (,sonokai-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,sonokai-class (:inherit sonokai-line-number))
      (,sonokai-256-class (:inherit sonokai-line-number))))

   `(realgud-backtrace-number
     ((,sonokai-class (:foreground ,sonokai-yellow-d
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background))))

   `(erb-delim-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :background ,sonokai-256-background))))

   `(erb-exec-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background))))

   `(erb-exec-delim-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :background ,sonokai-256-background))))

   `(erb-out-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background))))

   `(erb-out-delim-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :background ,sonokai-256-background))))

   `(erb-comment-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background))))

   `(erb-comment-delim-face
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :background ,sonokai-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-background))))

   `(rst-level-2-face
     ((,sonokai-class (:background ,sonokai-cyan
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-cyan
                                       :foreground ,sonokai-256-background))))

   `(rst-level-3-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background))))

   `(rst-level-4-face
     ((,sonokai-class (:background ,sonokai-violet
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-violet
                                       :foreground ,sonokai-256-background))))

   `(rst-level-5-face
     ((,sonokai-class (:background ,sonokai-magenta
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-magenta
                                       :foreground ,sonokai-256-background))))

   `(rst-level-6-face
     ((,sonokai-class (:background ,sonokai-red
                                   :foreground ,sonokai-background))
      (,sonokai-256-class (:background ,sonokai-256-red
                                       :foreground ,sonokai-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(rpm-spec-doc-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(rpm-spec-ghost-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(rpm-spec-macro-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(rpm-spec-package-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(rpm-spec-section-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(rpm-spec-tag-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(rpm-spec-var-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :weight bold))))

   `(sh-escaped-newline
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   `(sh-heredoc
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,sonokai-class (:inherit highlight))
      (,sonokai-256-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   `(show-paren-mismatch
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   `(paren-face-mismatch
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   `(paren-face-no-match
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background
                                   :weight normal
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background
                                       :weight normal
                                       :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; smerge
   `(smerge-base
     ((,sonokai-class (:background ,sonokai-diff-blue-base))
      (,sonokai-256-class (:background ,sonokai-256-diff-blue-base))))
   `(smerge-upper
     ((,sonokai-class (:background ,sonokai-diff-red-base))
      (,sonokai-256-class (:background ,sonokai-256-diff-red-base))))
   `(smerge-lower
     ((,sonokai-class (:background ,sonokai-diff-green-base))
      (,sonokai-256-class (:background ,sonokai-256-diff-green-base))))
   ;; WARNING: defining this face will overwrite the next two when displaying a
   ;; smerge diff in a file.
   ;; `(smerge-refined-changed
   ;;    ((,sonokai-class (:background ,sonokai-diff-blue-emphasis))
   ;;      (,sonokai-256-class (:background ,sonokai-256-diff-blue-emphasis))))
   `(smerge-refined-added
     ((,sonokai-class (:background ,sonokai-diff-green-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-diff-green-emphasis))))
   `(smerge-refined-removed
     ((,sonokai-class (:background ,sonokai-diff-red-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-diff-red-emphasis))))

   ;; speedbar
   `(speedbar-button-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-comments))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-comments))))

   `(speedbar-directory-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-blue))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-blue))))

   `(speedbar-file-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-foreground))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-foreground))))

   `(speedbar-highlight-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :background ,sonokai-highlight-line))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :background ,sonokai-256-highlight-line))))

   `(speedbar-selected-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-yellow
                                :underline t))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-yellow
                                    :underline t))))

   `(speedbar-separator-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :background ,sonokai-blue
                                :foreground ,sonokai-background
                                :overline ,sonokai-cyan-lc))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :background ,sonokai-256-blue
                                    :foreground ,sonokai-256-background
                                    :overline ,sonokai-256-cyan-lc))))

   `(speedbar-tag-face
     ((,sonokai-class (:inherit ,sonokai-pitch
                                :foreground ,sonokai-green))
      (,sonokai-256-class (:inherit ,sonokai-pitch
                                    :foreground ,sonokai-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,sonokai-class (:background ,sonokai-blue
                                   :foreground ,sonokai-background
                                   :height ,sonokai-height-plus-1
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-blue
                                       :foreground ,sonokai-256-background
                                       :height ,sonokai-height-plus-1
                                       :weight bold))))

   `(sr-editing-path-face
     ((,sonokai-class (:background ,sonokai-yellow
                                   :foreground ,sonokai-background
                                   :weight bold
                                   :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:background ,sonokai-256-yellow
                                       :foreground ,sonokai-256-background
                                       :weight bold
                                       :height ,sonokai-height-plus-1))))

   `(sr-highlight-path-face
     ((,sonokai-class (:background ,sonokai-green
                                   :foreground ,sonokai-background
                                   :weight bold
                                   :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:background ,sonokai-256-green
                                       :foreground ,sonokai-256-background
                                       :weight bold
                                       :height ,sonokai-height-plus-1))))

   `(sr-passive-path-face
     ((,sonokai-class (:background ,sonokai-comments
                                   :foreground ,sonokai-background
                                   :weight bold
                                   :height ,sonokai-height-plus-1))
      (,sonokai-256-class (:background ,sonokai-256-comments
                                       :foreground ,sonokai-256-background
                                       :weight bold
                                       :height ,sonokai-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,sonokai-class (:inherit disonokai-red-marked))
      (,sonokai-256-class (:inherit disonokai-red-marked))))

   `(sr-marked-file-face
     ((,sonokai-class (:inherit disonokai-red-marked))
      (,sonokai-256-class (:inherit disonokai-red-marked))))

   `(sr-alt-marked-dir-face
     ((,sonokai-class (:background ,sonokai-magenta
                                   :foreground ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-magenta
                                       :foreground ,sonokai-256-background
                                       :weight bold))))

   `(sr-alt-marked-file-face
     ((,sonokai-class (:background ,sonokai-magenta
                                   :foreground ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-magenta
                                       :foreground ,sonokai-256-background
                                       :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,sonokai-class (:inherit disonokai-red-directory
                                :weight normal))
      (,sonokai-256-class (:inherit disonokai-red-directory
                                    :weight normal))))

   `(sr-symlink-directory-face
     ((,sonokai-class (:inherit disonokai-red-directory
                                :slant italic
                                :weight normal))
      (,sonokai-256-class (:inherit disonokai-red-directory
                                    :slant italic
                                    :weight normal))))

   `(sr-symlink-face
     ((,sonokai-class (:inherit disonokai-red-symlink
                                :slant italic
                                :weight normal))
      (,sonokai-256-class (:inherit disonokai-red-symlink
                                    :slant italic
                                    :weight normal))))

   `(sr-broken-link-face
     ((,sonokai-class (:inherit disonokai-red-warning
                                :slant italic
                                :weight normal))
      (,sonokai-256-class (:inherit disonokai-red-warning
                                    :slant italic
                                    :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(sr-encrypted-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(sr-log-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(sr-packaged-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(sr-html-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(sr-xml-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,sonokai-class (:background ,sonokai-red
                                   :foreground ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:background ,sonokai-256-red
                                       :foreground ,sonokai-256-background
                                       :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-yellow))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-yellow))))

   `(syslog-hour-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-green))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-green))))

   `(syslog-error-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-red
                                       :weight bold))))

   `(syslog-warn-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-orange
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-orange
                                       :weight bold))))

   `(syslog-info-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-blue
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-blue
                                       :weight bold))))

   `(syslog-debug-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-cyan
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-cyan
                                       :weight bold))))

   `(syslog-su-face
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-magenta))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-magenta))))

   ;; table
   `(table-cell
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :background ,sonokai-highlight-line))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :background ,sonokai-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,sonokai-class (:foreground ,sonokai-gray
                                   :background ,sonokai-gray-d))
      (,sonokai-256-class (:foreground ,sonokai-256-gray
                                       :background ,sonokai-256-gray-d))))

   `(term-color-red
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-red-d))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-red-d))))

   `(term-color-green
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-green-d))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-green-d))))

   `(term-color-yellow
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-yellow-d))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-yellow-d))))

   `(term-color-blue
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-blue-d))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-blue-d))))

   `(term-color-magenta
     ((,sonokai-class (:foreground ,sonokai-magenta
                                   :background ,sonokai-magenta-d))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :background ,sonokai-256-magenta-d))))

   `(term-color-cyan
     ((,sonokai-class (:foreground ,sonokai-cyan
                                   :background ,sonokai-cyan-d))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan
                                       :background ,sonokai-256-cyan-d))))

   `(term-color-white
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-foreground))))

   `(term-default-fg-color
     ((,sonokai-class (:inherit term-color-white))
      (,sonokai-256-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,sonokai-class (:inherit term-color-black))
      (,sonokai-256-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,sonokai-class (:background ,sonokai-background-d
                                   :foreground ,sonokai-emphasis
                                   :inherit ,sonokai-pitch))))

   ;; treemacs
   `(treemacs-directory-face
     ((,sonokai-class (:foreground ,sonokai-violet
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-violet
                                       :background ,sonokai-256-background
                                       :weight bold))))

   `(treemacs-header-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-background
                                   :underline t
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-background
                                       :underline t
                                       :weight bold))))

   `(treemacs-git-modified-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background))))

   `(treemacs-git-renamed-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background))))

   `(treemacs-git-ignored-face
     ((,sonokai-class (:foreground ,sonokai-gray-l
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-l
                                       :background ,sonokai-256-background))))

   `(treemacs-git-untracked-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :background ,sonokai-256-background))))

   `(treemacs-git-added-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :background ,sonokai-256-background))))

   `(treemacs-git-conflict-face
     ((,sonokai-class (:foreground ,sonokai-orange
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-orange
                                       :background ,sonokai-256-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,sonokai-class (:foreground ,sonokai-magenta
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta
                                       :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :background ,sonokai-highlight-line
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :background ,sonokai-256-highlight-line
                                       :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,sonokai-class (:foreground ,sonokai-emphasis))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :background ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :background ,sonokai-256-red
                                       :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,sonokai-class (:foreground ,sonokai-cyan))
      (,sonokai-256-class (:foreground ,sonokai-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-background))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(undo-tree-visualizer-current-face
     ((,sonokai-class (:foreground ,sonokai-blue
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-blue
                                       :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :background ,sonokai-background
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :background ,sonokai-256-background
                                       :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,sonokai-class (:background ,sonokai-highlight-alt))
      (,sonokai-256-class (:background ,sonokai-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,sonokai-class (:inherit link))
      (,sonokai-256-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,sonokai-class (:inherit link-visited))
      (,sonokai-256-class (:inherit link-visited))))

   `(w3m-form
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground))))

   `(w3m-header-line-location-title
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-yellow))))

   `(w3m-header-line-location-content

     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   `(w3m-bold
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :weight bold))))

   `(w3m-image-anchor
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-cyan
                                   :inherit link))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-cyan
                                       :inherit link))))

   `(w3m-image
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-cyan))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,sonokai-class (:foreground ,sonokai-emphasis))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis))))

   `(w3m-lnum-match
     ((,sonokai-class (:background ,sonokai-highlight-line))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line))))

   `(w3m-lnum
     ((,sonokai-class (:underline nil
                                  :bold nil
                                  :foreground ,sonokai-red))
      (,sonokai-256-class (:underline nil
                                      :bold nil
                                      :foreground ,sonokai-256-red))))

   `(w3m-session-select
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(w3m-session-selected
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :bold t
                                   :underline t))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :bold t
                                       :underline t))))

   `(w3m-tab-background
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground))))

   `(w3m-tab-selected-background
     ((,sonokai-class (:background ,sonokai-background
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-background
                                       :foreground ,sonokai-256-foreground))))

   `(w3m-tab-mouse
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-yellow))))

   `(w3m-tab-selected
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-emphasis
                                   :bold t))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-emphasis
                                       :bold t))))

   `(w3m-tab-unselected
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-red))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-orange))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :foreground ,sonokai-violet))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :foreground ,sonokai-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(web-mode-comment-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(web-mode-constant-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,sonokai-class (:underline unspecified
                                  :weight unspecified
                                  :background ,sonokai-highlight-line))
      (,sonokai-256-class (:underline unspecified
                                      :weight unspecified
                                      :background ,sonokai-256-highlight-line))))

   `(web-mode-doctype-face
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :slant italic
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :slant italic
                                       :weight bold))))

   `(web-mode-folded-face
     ((,sonokai-class (:underline t))
      (,sonokai-256-class (:underline t))))

   `(web-mode-function-name-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(web-mode-html-attr-name-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,sonokai-class (:inherit web-mode-html-attr-name-face))
      (,sonokai-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,sonokai-class (:inherit web-mode-block-delimiter-face))
      (,sonokai-256-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,sonokai-class (:inherit web-mode-html-attr-name-face))
      (,sonokai-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(web-mode-html-tag-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(web-mode-html-tag-bracket-face
     ((,sonokai-class (:foreground ,sonokai-gray))
      (,sonokai-256-class (:foreground ,sonokai-256-gray))))

   `(web-mode-keyword-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(web-mode-preprocessor-face
     ((,sonokai-class (:foreground ,sonokai-yellow
                                   :slant normal
                                   :weight unspecified))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow
                                       :slant normal
                                       :weight unspecified))))

   `(web-mode-string-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(web-mode-type-face
     ((,sonokai-class (:inherit font-lock-type-face))
      (,sonokai-256-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(web-mode-warning-face
     ((,sonokai-class (:inherit font-lock-warning-face))
      (,sonokai-256-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,sonokai-class (:background unspecified))
      (,sonokai-256-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,sonokai-class (:inherit font-lock-preprocessor-face))
      (,sonokai-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,sonokai-class (:inherit web-mode-comment-face))
      (,sonokai-256-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,sonokai-class (:inherit font-lock-preprocessor-face))
      (,sonokai-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,sonokai-class (:inherit web-mode-string-face))
      (,sonokai-256-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,sonokai-class (:box 1 :weight bold))
      (,sonokai-256-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,sonokai-class (:inherit font-lock-constant-face))
      (,sonokai-256-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,sonokai-class (:inherit font-lock-builtin-face))
      (,sonokai-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,sonokai-class (:inherit font-lock-builtin-face))
      (,sonokai-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,sonokai-class (:inherit font-lock-function-name-face))
      (,sonokai-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,sonokai-class (:inherit font-lock-builtin-face))
      (,sonokai-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,sonokai-class (:inherit font-lock-function-name-face))
      (,sonokai-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,sonokai-class (:inherit font-lock-builtin-face))
      (,sonokai-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,sonokai-class (:inherit font-lock-variable-name-face))
      (,sonokai-256-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,sonokai-class (:inherit font-lock-keyword-face))
      (,sonokai-256-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,sonokai-class (:inherit web-mode-string-face))
      (,sonokai-256-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,sonokai-class (:inherit web-mode-string-face))
      (,sonokai-256-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,sonokai-class (:inherit web-mode-comment-face))
      (,sonokai-256-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(web-mode-json-key-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(web-mode-json-string-face
     ((,sonokai-class (:inherit web-mode-string-face))
      (,sonokai-256-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(web-mode-part-comment-face
     ((,sonokai-class (:inherit web-mode-comment-face))
      (,sonokai-256-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,sonokai-class (:inherit web-mode-block-face))
      (,sonokai-256-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,sonokai-class (:inherit web-mode-string-face))
      (,sonokai-256-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,sonokai-class (:foreground ,sonokai-violet))
      (,sonokai-256-class (:foreground ,sonokai-256-violet))))

   `(web-mode-whitespace-face
     ((,sonokai-class (:background ,sonokai-red))
      (,sonokai-256-class (:background ,sonokai-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-comments
                                       :inverse-video unspecified
                                       :slant italic))))

   `(whitespace-hspace
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-emphasis
                                   :inverse-video unspecified))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-emphasis
                                       :inverse-video unspecified))))

   `(whitespace-tab
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-red
                                       :inverse-video unspecified
                                       :weight bold))))

   `(whitespace-newline
     ((,sonokai-class(:background unspecified
                                  :foreground ,sonokai-comments
                                  :inverse-video unspecified))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-orange-lc
                                   :inverse-video t))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-orange-lc
                                       :inverse-video t))))

   `(whitespace-line
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-magenta
                                   :inverse-video unspecified))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-magenta
                                       :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,sonokai-class (:background ,sonokai-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,sonokai-256-class (:background ,sonokai-256-red-lc
                                       :foreground unspecified
                                       :inverse-video unspecified))))

   `(whitespace-indentation
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-yellow
                                       :inverse-video unspecified
                                       :weight bold))))

   `(whitespace-empty
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-red-lc
                                   :inverse-video t))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-red-lc
                                       :inverse-video t))))

   `(whitespace-space-after-tab
     ((,sonokai-class (:background unspecified
                                   :foreground ,sonokai-orange
                                   :inverse-video t
                                   :weight bold))
      (,sonokai-256-class (:background unspecified
                                       :foreground ,sonokai-256-orange
                                       :inverse-video t
                                       :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(wl-highlight-folder-many-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(wl-highlight-folder-path-face
     ((,sonokai-class (:foreground ,sonokai-orange))
      (,sonokai-256-class (:foreground ,sonokai-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-message-citation-header
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-headers-face
     ((,sonokai-class (:foreground ,sonokai-red))
      (,sonokai-256-class (:foreground ,sonokai-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-header-contents
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-signature
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(wl-highlight-summary-answesonokai-red-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,sonokai-class (:foreground ,sonokai-foreground
                                   :slant italic))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground
                                       :slant italic))))

   `(wl-highlight-summary-new-face
     ((,sonokai-class (:foreground ,sonokai-blue))
      (,sonokai-256-class (:foreground ,sonokai-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,sonokai-class (:foreground ,sonokai-yellow))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,sonokai-class (:foreground ,sonokai-magenta))
      (,sonokai-256-class (:foreground ,sonokai-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,sonokai-class (:underline t
                                  :weight bold))
      (,sonokai-256-class (:underline t
                                      :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,sonokai-class (:inherit error))
      (,sonokai-256-class (:inherit error))))

   `(weechat-highlight-face
     ((,sonokai-class (:foreground ,sonokai-emphasis
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-emphasis
                                       :weight bold))))

   `(weechat-nick-self-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight unspecified
                                   :inverse-video t))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight unspecified
                                       :inverse-video t))))

   `(weechat-prompt-face
     ((,sonokai-class (:inherit minibuffer-prompt))
      (,sonokai-256-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,sonokai-class (:foreground ,sonokai-green
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-green
                                       :weight bold))))

   `(which-key-separator-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(which-key-note-face
     ((,sonokai-class (:foreground ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments))))

   `(which-key-command-description-face
     ((,sonokai-class (:foreground ,sonokai-foreground))
      (,sonokai-256-class (:foreground ,sonokai-256-foreground))))

   `(which-key-local-map-description-face
     ((,sonokai-class (:foreground ,sonokai-yellow-hc))
      (,sonokai-256-class (:foreground ,sonokai-256-yellow-hc))))

   `(which-key-group-description-face
     ((,sonokai-class (:foreground ,sonokai-red
                                   :weight bold))
      (,sonokai-256-class (:foreground ,sonokai-256-red
                                       :weight bold))))

   ;; window-divider-mode
   `(window-divider
     ((,sonokai-class (:foreground ,sonokai-highlight))
      (,sonokai-256-class (:foreground ,sonokai-highlight))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; window-number-mode
   `(window-number-face
     ((,sonokai-class (:foreground ,sonokai-green))
      (,sonokai-256-class (:foreground ,sonokai-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-comments))))

   `(yascroll:thumb-fringe
     ((,sonokai-class (:foreground ,sonokai-comments
                                   :background ,sonokai-comments))
      (,sonokai-256-class (:foreground ,sonokai-256-comments
                                       :background ,sonokai-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,sonokai-class (:background ,sonokai-highlight-line
                                   :box ,sonokai-emphasis))
      (,sonokai-256-class (:background ,sonokai-256-highlight-line
                                       :box ,sonokai-256-emphasis))))

   ;; corfu
   `(corfu-default
     ((,sonokai-class (:background ,sonokai-background-d))
      (,sonokai-256-class (:background ,sonokai-256-background-d))))

   `(corfu-current
     ((,sonokai-class (:background ,sonokai-highlight
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight
                                       :foreground ,sonokai-256-foreground))))

   `(corfu-border
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   `(corfu-bar
     ((,sonokai-class (:background ,sonokai-yellow))
      (,sonokai-256-class (:background ,sonokai-256-yellow))))

   ;; corfu-candidate-overlay
   `(corfu-candidate-overlay-face
     ((,sonokai-class (:foreground ,sonokai-gray-d))
      (,sonokai-256-class (:foreground ,sonokai-256-gray-d))))

   `(corfu-candidate-overlay-face-exact-match
     ((,sonokai-class (:foreground ,sonokai-white-d))
      (,sonokai-256-class (:foreground, sonokai-256-white-d))))

   ;; mozc-posframe
   `(mozc-cand-posframe-normal-face
     ((,sonokai-class (:background ,sonokai-background-d
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-background-d
                                       :foreground ,sonokai-foreground))))

   `(mozc-cand-posframe-focused-face
     ((,sonokai-class (:background ,sonokai-highlight
                                   :foreground ,sonokai-foreground))
      (,sonokai-256-class (:background ,sonokai-256-highlight
                                       :foreground ,sonokai-256-foreground))))

   `(mozc-cand-posframe-footer-face
     ((,sonokai-class (:inherit mozc-cand-posframe-normal-face
                                :foreground ,sonokai-yellow))
      (,sonokai-256-class (:inherit mozc-cand-posframe-normal-face
                                    :foreground ,sonokai-256-yellow))))

   `(mozc-cand-posframe-border-face
     ((,sonokai-class (:background ,sonokai-gray))
      (,sonokai-256-class (:background ,sonokai-256-gray))))

   ;; ext-tab-bar
   `(ext-tab-bar-faces-debug
     ((,sonokai-class (:foreground ,sonokai-red-d))
      (,sonokai-256-class (:foreground ,sonokai-256-red-d))))

   `(ext-tab-bar-faces-project
     ((,sonokai-class (:foreground ,sonokai-green-d))
      (,sonokai-256-class (:foreground ,sonokai-256-green-d))))

   `(ext-tab-bar-faces-perspective
     ((,sonokai-class (:foreground ,sonokai-orange-d))
      (,sonokai-256-class (:foreground ,sonokai-256-orange-d))))
   )

  (custom-theme-set-variables
   'sonokai
   `(ansi-color-names-vector [,sonokai-background ,sonokai-red ,sonokai-green ,sonokai-yellow
                                                  ,sonokai-blue ,sonokai-magenta ,sonokai-cyan ,sonokai-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,sonokai-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,sonokai-magenta ,sonokai-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,sonokai-highlight-line . 0)
       (,sonokai-green-lc . 20)
       (,sonokai-cyan-lc . 30)
       (,sonokai-blue-lc . 50)
       (,sonokai-yellow-lc . 60)
       (,sonokai-orange-lc . 70)
       (,sonokai-magenta-lc . 85)
       (,sonokai-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,sonokai-emphasis)
   `(pos-tip-background-color ,sonokai-bg-alt-d)

   ;; vc
   `(vc-annotate-color-map
     '((20 . "#870000")
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . "#CDC673")
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . "#AFD700")
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . "#5FAFAF")
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . "#0084DC")))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,sonokai-background ,sonokai-highlight-line
                   ,sonokai-red-d ,sonokai-red
                   ,sonokai-green-d ,sonokai-green
                   ,sonokai-yellow-d ,sonokai-yellow
                   ,sonokai-blue-d ,sonokai-blue
                   ,sonokai-magenta-d ,sonokai-magenta
                   ,sonokai-cyan-d ,sonokai-cyan
                   ,sonokai-foreground ,sonokai-emphasis)))
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sonokai)

;;; sonokai-theme.el ends here
