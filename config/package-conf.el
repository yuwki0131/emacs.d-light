;;; package-conf.el --- packages
;;; Commentary:
;;;  Emacsインストールパッケージ(package-install要) / package-conf.el
;;;  package func : 機能
;;;  package edit : 編集
;;;  package app : 外観
;;;  package search : 検索
;;;  package config : 設定
;;;  package report : 報告・計測
;;; Code:

;;; --------------------------------------------------------------------------------
;;; package-func : functions
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-func : magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package-with-report magit)

;;; ---------------------------------------------------------------------------
;;; package-func : magit : emacs git client
;;; ---------------------------------------------------------------------------
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; package-func : server : emacs client/server
;;; ---------------------------------------------------------------------------
(use-package-with-report server
  :config
  (unless (server-running-p)
    (server-start)))

;;; ---------------------------------------------------------------------------
;;; package-func : auto compile : elファイル自動コンパイル
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; ---------------------------------------------------------------------------
;;; package-func : auto complete : 自動補完
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-complete
  :config
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-candidate-max 40)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

;;; ---------------------------------------------------------------------------
;;; package-func : google this : ググる
;;; ---------------------------------------------------------------------------
(use-package-with-report google-this
  :config
  (google-this-mode t)
  (setq google-this-location-suffix "co.jp")
  (defun google-this-url ()
    "URL for google searches."
    (concat google-this-base-url google-this-location-suffix
	    "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja")))

;;; ---------------------------------------------------------------------------
;;; package-func : w3m : w3m in emacs
;;; ---------------------------------------------------------------------------
(use-package-with-report w3m
  :config
  (setq w3m-command "w3m"))

;;; ---------------------------------------------------------------------------
;;; package-func : auto async byte compile : emacsのバイトコンパイルの自動化
;;; ---------------------------------------------------------------------------
(use-package-with-report auto-async-byte-compile)

;;; ---------------------------------------------------------------------------
;;; package-func : tiny menu : tiny menu
;;; ---------------------------------------------------------------------------
(use-package-with-report tiny-menu)

;;; ---------------------------------------------------------------------------
;;; package-func : imenu list : 関数定義を横に表示
;;; ---------------------------------------------------------------------------
(use-package-with-report imenu-list)

;;; ---------------------------------------------------------------------------
;;; package-func : browse kill ring : kill ring
;;; ---------------------------------------------------------------------------
(use-package-with-report browse-kill-ring)

;;; ---------------------------------------------------------------------------
;;; package-func : neotree : ディレクトリ表示
;;; ---------------------------------------------------------------------------
(use-package-with-report neotree
  :config
  (setq neo-show-hidden-files t)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (setq neo-theme 'ascii)
  (add-hook 'neotree-mode-hook '(lambda () (nlinum-mode -1))))

;;; ---------------------------------------------------------------------------
;;; package-func : open junk file : ジャンクファイル生成
;;; ---------------------------------------------------------------------------
(use-package-with-report open-junk-file
  :config
  (setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S."))

;;; ---------------------------------------------------------------------------
;;; package-func : twittering mode : ついった
;;; ---------------------------------------------------------------------------
(use-package-with-report twittering-mode
  :config
  (setq twittering-use-master-password t)
  ;; 更新頻度(sec)
  (setq twittering-timer-interval 30)
  ;; 単位時間あたりのツイート取得数
  (setq twittering-number-of-tweets-on-retrieval 50)
  ;; アイコン表示
  (setq twittering-icon-mode t)
  ;; 表示形式
  (setq twittering-status-format "%i @%s %S %p: \n %T
──────────────────────────────────────────────────────────────────────────"))

;;; ---------------------------------------------------------------------------
;;; package-func : markdown preview mode : markdown preview
;;; ---------------------------------------------------------------------------
;; required apt-get install markdown
(use-package-with-report markdown-preview-mode
  :config
  (setq markdown-command "/usr/bin/marked"))

;;; ---------------------------------------------------------------------------
;;; package-func : codic : j2e/e2j dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report codic)

;;; ---------------------------------------------------------------------------
;;; package-func : codic : e2e dictionary
;;; ---------------------------------------------------------------------------
(use-package-with-report define-word)

;;; ---------------------------------------------------------------------------
;;; package-func : hide comnt : hide comment
;;; ---------------------------------------------------------------------------
(use-package-with-report hide-comnt)

;;; ---------------------------------------------------------------------------
;;; package-func : which key : キーバインドサポート
;;; ---------------------------------------------------------------------------
(use-package-with-report which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;; ---------------------------------------------------------------------------
;;; package-move : goto chg : 最後に変更した箇所へカーソルを移動
;;; ---------------------------------------------------------------------------
(use-package-with-report goto-chg)

;;; ---------------------------------------------------------------------------
;;; package-move : point-undo : カーソル位置を戻す
;;; ---------------------------------------------------------------------------
(use-package-with-report point-undo)

;;; ---------------------------------------------------------------------------
;;; package-move : popwin : ヘルプ/補完バッファをポップアップで表示
;;; ---------------------------------------------------------------------------
(use-package-with-report popwin
  :config
  (add-hook 'prog-mode-hook 'popwin-mode)
  (setq display-buffer-function 'popwin:display-buffer))

;;; --------------------------------------------------------------------------------
;;; package-edit : edit something
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-edit : redo+ : 普通のredo
;;; ---------------------------------------------------------------------------
(use-package-with-report redo+
  :config
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

;;; ---------------------------------------------------------------------------
;;; package-edit : smart newline : 改行
;;; ---------------------------------------------------------------------------
(use-package-with-report smart-newline
  :config
  (smart-newline-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-edit : edit grepped text : grep済みのテキスト編集、反映
;;; ---------------------------------------------------------------------------
(use-package-with-report wgrep
  :config
  ;; "e"でwgrepモード有効
  (setf wgrep-enable-key "e")
  ;; wgrep終了時にバッファを保存
  (setq wgrep-auto-save-buffer t))

;;; ---------------------------------------------------------------------------
;;; package-edit : hungry-delete-mode : 空白の貪欲な削除
;;; ---------------------------------------------------------------------------
(use-package-with-report hungry-delete
  :disabled t
  :config
  (global-hungry-delete-mode))

;;; ---------------------------------------------------------------------------
;;; package-edit : drug-stuff : 単語単位で移動
;;; ---------------------------------------------------------------------------
(use-package-with-report drag-stuff
  :config
  (setq drag-stuff-modifier '(meta shift))
  (drag-stuff-global-mode t))

;;; ---------------------------------------------------------------------------
;;; package-edit : multiple-cursors-mode : multiple-cursors-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report multiple-cursors)

;;; ---------------------------------------------------------------------------
;;; package-edit : iedit-mode
;;; ---------------------------------------------------------------------------
(use-package-with-report iedit)

;;; --------------------------------------------------------------------------------
;;; package-app : appearance something
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-app : bm : 現在行を永続的に記憶
;;; ---------------------------------------------------------------------------
(use-package-with-report bm)

;;; ---------------------------------------------------------------------------
;;; package-app : fancy-narrow : 現在の領域をnarrowingする
;;; ---------------------------------------------------------------------------
(use-package-with-report fancy-narrow
  :config
  (fancy-narrow-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-app : にゃーん
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation))

;;; ---------------------------------------------------------------------------
;;; package-app : echo bell : from beep to echo
;;; ---------------------------------------------------------------------------
;; original : https://www.youtube.com/watch?v=QH2-TGUlwu4
(use-package-with-report echo-bell
  :config
  (setq echo-bell-string "(´・_・｀)")
  (setq echo-bell-background "lightpink")
  (setq echo-bell-delay 0.4)
  (echo-bell-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-app : nlinum-mode : 軽量化された行番号表示
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない
(use-package-with-report nlinum
  :config
  (global-nlinum-mode t)
  (setq nlinum-format " %4d "))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight line plus : カーソル行ハイライト(拡張)
;;; ---------------------------------------------------------------------------
;; 標準は重いので使用しない。以下を使用
(use-package-with-report hl-line+
  :config
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3))

;;; ---------------------------------------------------------------------------
;;; package-app : column highlight line plus : カーソル桁ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report col-highlight
  :disabled t
  :config
  (toggle-highlight-column-when-idle 1)
  (col-highlight-set-interval 3)
  (column-highlight-mode nil))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight current-buffer : 現在のバッファをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report hiwin
  :disabled t
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "#D0D0D0"))

;;; ---------------------------------------------------------------------------
;;; package-app : hl-todo-mode : TODOをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report hl-todo
  :init
  (setq hl-todo-keyword-faces '(("TODO" . "#cc9393")))
  :config
  ;;; global-hl-todo-modeで有効にするメジャーモード(derived-mode)
  (setq hl-todo-activate-in-modes '(prog-mode))
  (global-hl-todo-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight indententation-mode : インデント表示
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#ffffff")
  (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;;; ---------------------------------------------------------------------------
;;; package-app : volatile highlights : 修正箇所のハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report volatile-highlights
  :config
  (volatile-highlights-mode t))

;;; ---------------------------------------------------------------------------
;;; package-app : beacon : bufferを移動時にハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report beacon
  :config
  (beacon-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight symbol : カーソル位置のシンボルの自動ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-symbol
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; ---------------------------------------------------------------------------
;;; package-app : highlight block : 現在のブロックをハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report highlight-blocks
  :disabled
  :config
  (add-hook 'prog-mode-hook '(lambda () (highlight-blocks-mode 1))))

;;; ---------------------------------------------------------------------------
;;; package-app : rainbow delimiters : 対応括弧ハイライト
;;; ---------------------------------------------------------------------------
(use-package-with-report rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; deriving from
  ;; https://yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial
  (require 'cl-lib)
  (require 'color)
  (let ((index 1))
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

;;; ---------------------------------------------------------------------------
;;; package-app : anzu : モードラインの左側に検索中の単語数を表示
;;; ---------------------------------------------------------------------------
(use-package-with-report anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

;;; ---------------------------------------------------------------------------
;;; package-app : path header line mode : path header line mode
;;; ---------------------------------------------------------------------------
(use-package-with-report path-headerline-mode
  :config
  (path-headerline-mode +1))

;;; ---------------------------------------------------------------------------
;;; package-app : sublimity : sublime風アウトライン表示
;;; ---------------------------------------------------------------------------
(use-package-with-report sublimity
  :config
  (require 'sublimity-scroll)
  ;; これも基本的に不要
  ;; (require 'sublimity-map)
  ;; 表示領域が狭くなるため基本的に不要
  ;; (require 'sublimity-attractive)
  (add-hook 'prog-mode-hook '(lambda () (sublimity-mode 1)))
  (setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5))

;;; --------------------------------------------------------------------------------
;;; package-search : search
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-search : visual regexp steroids : 正規表現の拡張
;;; ---------------------------------------------------------------------------
(use-package-with-report visual-regexp-steroids
  :config
  (setq vr/engine 'java))

;;; ---------------------------------------------------------------------------
;;; package-search : helm : helm
;;; ---------------------------------------------------------------------------
(use-package-with-report helm
   :diminish helm-mode
   :config
  (require 'helm-config)
  (helm-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-search : zop-to-char : M-zの可視化
;;; ---------------------------------------------------------------------------
(use-package-with-report zop-to-char)

;;; ---------------------------------------------------------------------------
;;; package-search : swoop : トークンレベル移動(検索系)
;;; ---------------------------------------------------------------------------
(use-package-with-report swoop)

;;; ---------------------------------------------------------------------------
;;; package-search : ace jump mode : 任意の場所に3ストロークで移動
;;; ---------------------------------------------------------------------------
(use-package-with-report ace-jump-mode)

;;; ---------------------------------------------------------------------------
;;; package-search : subword mode : Camel notationのシンボル移動時の単位を変更
;;; ---------------------------------------------------------------------------
;;; (before) |ITransientAssociative| -> (after) |I|Transient|Associative|
(use-package-with-report subword
  :config (global-subword-mode +1))

;;; ---------------------------------------------------------------------------
;;; package-search : migemo : isearchをローマ字のままで日本語も検索可能に
;;; ---------------------------------------------------------------------------
;; required : sudo apt-get install cmigemo
(use-package-with-report migemo
  :config
  (when (executable-find "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))

;;; --------------------------------------------------------------------------------
;;; package-config : configuration
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; package-config : mouse disable : マウス禁止
;;; ---------------------------------------------------------------------------
(use-package-with-report disable-mouse
  :diminish disable-mouse-mode
  :config
  (global-disable-mouse-mode))

;;; ---------------------------------------------------------------------------
;;; package-config : exec path from shell : シェルのpath設定引き継ぎ
;;; ---------------------------------------------------------------------------
(use-package-with-report exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; package-report : esup : emacs起動時間の計測
;;; ---------------------------------------------------------------------------
(use-package-with-report esup)

;;; ---------------------------------------------------------------------------
;;; package-report : key freq mode : keyfreq
;;; ---------------------------------------------------------------------------
(use-package-with-report keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; ---------------------------------------------------------------------------
;;; package-report : key freq mode : keyfreq
;;; ---------------------------------------------------------------------------
(use-package-with-report aozora-view)

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'package-conf)
;;; package-conf.el ends here
