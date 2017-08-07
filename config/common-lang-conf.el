;;; package --- common-lang-conf.el (common settings for languages)
;;; Commentary:
;;;  複数言語共通の設定
;;; Code:

;;; ---------------------------------------------------------------------------
;;; yasnippet : スニペット
;;; ---------------------------------------------------------------------------
;; see : https://github.com/AndreaCrotti/yasnippet-snippets
(use-package-with-report yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/mysnippets"
	  "~/.emacs.d/yasnippets"))
  (yas-global-mode t))

;; memo :
;; 新規スニペット作成バッファを用意する
;; (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
;; (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;;; ---------------------------------------------------------------------------
;;; origami : ford the inner of parenthesis
;;; ---------------------------------------------------------------------------
(use-package-with-report origami)

;;; ---------------------------------------------------------------------------
;;; highlight numbers : 数値のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;; ---------------------------------------------------------------------------
;;; highlight operators : 演算子のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-operators
  :config
  (add-hook 'python-mode-hook 'highlight-operators-mode)
  (add-hook 'lua-mode-hook 'highlight-operators-mode))

;;; ---------------------------------------------------------------------------
;;; flymake : 文法チェッカ
;;; ---------------------------------------------------------------------------
(use-package-with-report flycheck)

(use-package-with-report flymake-cursor)

;;; ---------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------
(provide 'common-lang-conf)
;;; common-lang-conf.el ends here
