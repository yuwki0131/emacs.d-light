;;; package --- language-conf.el (specific settings for languages)
;;; Commentary:
;;;  言語設定 / language-conf.el
;;;  Functional Programming Languages
;;;  - Scala
;;;  - Haskell
;;;  Lightweight Languages
;;;  - Python
;;;  Procedual Programming Languages
;;;  - nothing
;;;  Other Languages
;;;  - Web
;;; Code:

;;; --------------------------------------------------------------------------------
;;; config : Functional programming languages
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; scala-mode : Scala (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report scala-mode)

(use-package-with-report ensime
  :config
  (setq ensime-completion-style 'auto-complete)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;;; ---------------------------------------------------------------------------
;;; haskell-mode : Haskell (checked)
;;; ---------------------------------------------------------------------------
(use-package-with-report haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode))
  :config
  (setq haskell-program-name "ghci"))

(ignore-require-with-report
 "failed tot load inf-haskell, haskell-cabal"
 (use-package inf-haskell)
 (use-package haskell-cabal
   :mode (("\\.cabal$" . haskell-cabal-mode)))
 (setq haskell-font-lock-symbols t))

;;; --------------------------------------------------------------------------------
;;; config : LL
;;; --------------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; jedi, epc, elpy, flycheck : Python (unchecked)
;;; ---------------------------------------------------------------------------
(use-package-with-report python-environment)

(use-package-with-report jedi
  :bind (("\C-cd" . jedi:goto-definition)
         ("\C-cp" . jedi:goto-definition-pop-marker)
         ("\C-cr" . helm-jedi-related-names))
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package-with-report epc)
(use-package-with-report elpy)

;; PEP8 check
(use-package-with-report py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-enable-on-save))

(use-package-with-report flycheck-pyflakes
  :config
  (require 'tramp-cmds)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  (setq flymake-python-pyflakes-executable "/usr/bin/pyflakes")
  (setq flymake-gui-warnings-enabled t)
  (custom-set-variables
   '(flymake-python-pyflakes-extra-arguments
     (quote ("--max-line-length=120" "--ignore=E128"))))
  (defun flymake-pyflakes-init ()
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))

(defun my-python-mode ()
  (progn
    (setq indent-tabs-mode nil)
    (setq indent-level 4)
    (setq python-indent 4)
    (setq tab-width 4)
    (flycheck-mode t)
    (setq imenu-create-index-function 'python-imenu-create-index)
    ;; 元々のauto-complete補完候補を消す
    (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
    (add-to-list 'ac-sources 'ac-source-filename)
    (add-to-list 'ac-sources 'ac-source-jedi-direct)
    (flymake-mode t)))

(add-hook 'python-mode-hook 'my-python-mode)

;;; ---------------------------------------------------------------------------
;;; web-mode : web編集用
;;; ---------------------------------------------------------------------------
(use-package-with-report web-mode
  :mode
  (("\\.jsp$" . web-mode))
  (("\\.html$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;;; ---------------------------------------------------------------------------------
;;; provide
;;; ---------------------------------------------------------------------------------
(provide 'language-conf)
;;; language-conf.el ends here
