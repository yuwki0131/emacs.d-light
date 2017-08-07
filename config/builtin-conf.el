;;; package --- builtin-conf.el
;;; Commentary:
;;;  デフォルト機能の諸設定 / bizz-conf.el
;;; Code:

;; デフォルト起動時の画面非表示
(setq inhibit-startup-message t)

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー表示(Linux Mint/Cinnamonだと綺麗な表示)
(menu-bar-mode 1)

;; スクロールバー非表示
(scroll-bar-mode -1)

;; 画像ファイル表示
(auto-image-file-mode t)

;; 列数を表示
(column-number-mode t)

;; バッファ自動再読み込み
(global-auto-revert-mode t)

;; 行番号を表示(標準) => 使用しない
;; (global-linum-mode t)

;; scratchの初期のメッセージ消去
(setq initial-scratch-message nil)

;; beep音消す => ミニバッファへ反映
;; (setq visible-bell t)
;; (setq ring-bell-function 'ignore)

;; backupfile(*.~) つくらない
(setq make-backup-files nil)

;; backupfile(*.#) つくらない
(setq auto-save-default nil)

;; 折り返しを表示
(setq truncate-lines t)

;;  折り返しを表示(ウインドウ分割時)
(setq truncate-partial-width-windows nil)

;; マークのリージョンに色を付ける
(setq transient-mark-mode t)

;; スタートメッセージを非表示
(setq inhibit-startup-message t)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; 最終行に1行挿入
(setq require-final-newline t)

;; バッファの終端を表示(空行表示)
(setq-default indicate-empty-lines t)

;; カーソルタイプ
(setq default-cursor-type '(bar . 3))

;; 再帰的ミニバッファ
(setq enable-recursive-minibuffers t)

;; 再帰的ミニバッファの深さを表示
(minibuffer-depth-indicate-mode 1)

;; from yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ファイル保存時に空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ファイル読込補完、大文字/小文字無視
(setq read-file-name-completion-ignore-case t)

;; すべてのプログラムモードに対してタブ幅設定
(add-hook 'prog-mode-hook '(lambda () (setq tab-width 4)))

;; インデントタブ
(setq-default indent-tabs-mode nil)

;; URL強調表示、URLをブラウザで表示
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; ‘isearch-word’ is an obsolete variable (as of 25.1)対策
(setq search-default-regexp-mode nil)

;;; ---------------------------------------------------------------------------
;;; 別ディレクトリの同名バッファにディレクトリ名を付与する
;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'builtin-conf)
;;; bizz-conf.el ends here
