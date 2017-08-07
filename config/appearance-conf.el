;;; appearance-conf.el --- packages
;;; Commentary:
;;;  外観 / appearance-conf.el
;;; Code:

;;; ---------------------------------------------------------------------------
;;; フォント設定
;;; ---------------------------------------------------------------------------

(defvar default-font-family "Ubuntu Mono")
(defvar default-font-family-jp "Takaoゴシック")

;; デフォルトフォント設定
(set-face-attribute
 'default nil :family default-font-family :height 100)

;; 日本語のフォントセット : あいうえお ... 日本語
(set-fontset-font
 'nil 'japanese-jisx0208 (font-spec :family default-font-family-jp :height 90))

;; ギリシャ文字のフォントセット : αβγκλ ... ΛΩ
(set-fontset-font
 'nil '(#x0370 . #x03FF) (font-spec :family default-font-family :height 100))

;; キリル文字のフォントセット : Эта статья ... Русский
(set-fontset-font
 'nil '(#x0400 . #x04FF) (font-spec :family default-font-family :height 100))

;; 行間
(setq-default line-spacing 2)

;;; ---------------------------------------------------------------------------
;;; ハイライト系設定(標準)
;;; ---------------------------------------------------------------------------
;; M-x list-faces-display で編集対象の色を表示
;; 括弧の対応関係を色で示す
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq font-lock-maximum-decoration t)

;;; ---------------------------------------------------------------------------
;;; 色設定
;;; ---------------------------------------------------------------------------
;; cyan系
(defvar color/darkcyan "#0997B6")
(defvar color/lightcyan "lightcyan")

;; red系
(defvar color/red "red")
(defvar color/darkred "darkred")

;; magenta系
(defvar color/deeppink "deeppink")
(defvar color/purpledark "#9B8B9B")
(defvar color/lightpink "lightpink")

;; lime green系
(defvar color/limegreen "#32cd32")

;; gray系
(defvar color/active "#101010")
(defvar color/inactive "#393939")
(defvar color/bggray "#EFEFEF")
(defvar color/fggray "#101010")
(defvar color/comment "#777777")

;; 背景/前景の設定
(set-background-color color/bggray)
(set-foreground-color color/fggray)

;; 各構文要素の色/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-cbi (attr-symbol color-name bold-param italic-param)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold-p     attr-symbol bold-param)
  (set-face-italic-p   attr-symbol italic-param))

;; 各構文要素の色(foreground, background)/タイプ(normal/italic/bold)の設定
(defun set-face-fore-with-bfcbi (attr-symbol color-fg color-bg bold italic)
  (set-face-foreground attr-symbol color-fg)
  (set-face-background attr-symbol color-bg)
  (set-face-bold-p     attr-symbol bold)
  (set-face-italic-p   attr-symbol italic))

;; coloring program
(set-face-fore-with-cbi 'font-lock-comment-face       color/comment    nil t)
(set-face-fore-with-cbi 'font-lock-doc-face           color/deeppink   nil t)
(set-face-fore-with-cbi 'font-lock-string-face        color/limegreen  t   nil)
(set-face-fore-with-cbi 'font-lock-keyword-face       color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-builtin-face       color/darkcyan   nil nil)
(set-face-fore-with-cbi 'font-lock-function-name-face color/limegreen  t   nil)
(set-face-fore-with-cbi 'font-lock-variable-name-face color/limegreen  t   nil)
(set-face-fore-with-cbi 'font-lock-type-face          color/darkcyan   t   nil)
(set-face-fore-with-cbi 'font-lock-constant-face      color/deeppink   t   nil)
(set-face-fore-with-cbi 'font-lock-warning-face       color/deeppink   nil t)

;; coloring program (extension)
;; (set-face-fore-with-cbi 'highlight-numbers-number     color/deeppink   nil nil)
;; (set-face-fore-with-cbi 'highlight-operators-face     color/limegreen  nil nil)

;; coloring property
(ignore-errors
  (set-face-fore-with-cbi 'info-header-xref             color/darkcyan   nil t)
  (set-face-fore-with-cbi 'info-xref                    color/darkcyan   nil t)
  (set-face-fore-with-cbi 'link                         color/deeppink   nil t)
  (set-face-fore-with-cbi 'escape-glyph                 color/darkcyan   nil nil)
  (set-face-fore-with-cbi 'minibuffer-prompt            color/deeppink   t   nil))

;; coloring ac
(ignore-errors
  (set-face-fore-with-bfcbi 'ac-selection-face  color/deeppink "white"   t   nil)
  (set-face-fore-with-bfcbi 'ac-candidate-face  "white" color/darkcyan   t   nil))

;; カーソルの色
(set-cursor-color color/deeppink)

;; カーソル行ハイライト
(defface hlline-face
  `((((class color) (background dark))  (:background ,color/inactive))
    (((class color) (background light)) (:background ,color/lightcyan))
    (t ())) "*Face used by hl-line.")

(setq hl-line-face 'hlline-face)

;; カーソル桁ハイライト
(ignore-errors
  (custom-set-faces '(col-highlight ((t (:inherit hl-line))))))

;; 選択範囲
(set-face-foreground 'region "gray80")
(set-face-background 'region "gray20")

;; 行番号(line-num)の色の設定
(ignore-errors
  (set-face-attribute 'linum nil
                      :foreground "white" :background color/inactive
                      :weight 'bold))

;; モードラインの色の設定(active)
(ignore-errors
  (set-face-attribute 'mode-line nil
   :foreground color/lightcyan :background color/inactive
   :inverse-video nil
   :weight 'bold
   :height 100
   :font default-font-family
   :box '(:line-width 1 :color "black" :style nil)))

;; モードラインの色の設定(inactive)
(ignore-errors
  (set-face-attribute 'mode-line-inactive nil
   :foreground color/lightcyan :background color/inactive
   :inverse-video nil
   :weight 'extra-light
   :height 100
   :font default-font-family
   :box '(:line-width 1 :color "gray30" :style nil)))

(ignore-errors
  (custom-set-variables '(hl-sexp-background-color color/lightpink)))

;;;---------------------------------------------------------------------------
;;; ウィンドウ幅などの設定
;;;---------------------------------------------------------------------------
;; カーソルタイプ
(setq default-cursor-type '(bar . 2))

;; タイトルバー
(setq frame-title-format "emacs : %b")

;; モードライン(時間非表示)
(setq display-time-day-and-date -1)

;; 行番号フォーマット
(setq linum-format " %4d")

;; 画面サイズ初期化
(setq initial-frame-alist
      '((top . 20) (left . 0) (width . 128) (height . 75)  (alpha . (97 85))))

;;;---------------------------------------------------------------------------
;;; provide
;;;---------------------------------------------------------------------------
(provide 'appearance-conf)
