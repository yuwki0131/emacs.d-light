;;; package --- Emacs config-utils.el
;;; Commentary:
;;;  config-utils.el
;;;  - spit (export files)
;;;  - use-package-with-report
;;;  - global-safe-set-key
;;;  - gssk-report
;;;  - configuration report
;;;  - generate README.md
;;; Code:

;;; ---------------------------------------------------------------------------
;;; spit text to the file
;;; ---------------------------------------------------------------------------
(defun spit (file-name text)
  (ignore-errors
	(if (file-exists-p file-name)
		(delete-file file-name))
	(find-file file-name)
	(insert text)
	(save-buffer)
	(kill-buffer)
    t))

(defun slurp (file-name)
  (ignore-errors
	(if (not (file-exists-p file-name))
		(concat "file-not-found in slurp: " file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (buffer-string)))))

;;; ---------------------------------------------------------------------------
;;; interpose
;;; ---------------------------------------------------------------------------
(defun interpose (xs obj)
  (let ((size (length xs))
        (count 0)
        (ys (reverse xs))
        (zs '()))
    (while (not (null ys))
      (setq zs (cons obj (cons (car ys) zs)))
      (setq ys (cdr ys)))
    (cdr zs)))

;;; ---------------------------------------------------------------------------
;;; concat & interpose newline
;;; ---------------------------------------------------------------------------
(defun concat-interpose-newline (text-ls)
  (apply #'concat (interpose text-ls "\n")))

;;; ---------------------------------------------------------------------------
;;; interpose comment out
;;; ---------------------------------------------------------------------------
(defun add-comment-out (text)
  (concat ";;; " text))

(defun comment-out-message (text)
  (concat-interpose-newline
   (mapcar #'add-comment-out (split-string text "\n"))))

;;; ---------------------------------------------------------------------------
;;; failed-packages report : use-packageに失敗したパッケージのレポート
;;; ---------------------------------------------------------------------------
;; パッケージのLoading 状況をレポートする。 *scratch*バッファに結果出力
(defvar failed-packages '())

(defvar other-configuration-reports '())

(defmacro use-package-with-report (&rest body)
  `(progn
     (use-package . ,body)
     (when (not (package-installed-p (quote ,(car body))))
       (add-to-list 'failed-packages ,(symbol-name (car body))))))

(defmacro ignore-require-with-report (comment-if-failed &rest body)
  `(progn
     (if (not (ignore-errors . ,(append body '(t))))
         (setq other-configuration-reports
               (cons ,comment-if-failed other-configuration-reports)))))

(defun to-report-message (line)
  (concat "  - failed to load: " line))

(defun report-failed-packages ()
  (if (and (not failed-packages) (not other-configuration-reports))
      "all defined packages have been installed successfully"
    (concat
     "use-package-with-report error or not used packages: \n"
     (concat-interpose-newline
      (mapcar #'to-report-message failed-packages))
     (if failed-packages "\n" "")
     (concat-interpose-newline other-configuration-reports))))

(defun to-package-install-sexp (text)
  (concat "(package-install '" text ")"))

(defun generate-package-install-scenario ()
  (if failed-packages
      (let* ((failed-ls (mapcar #'to-package-install-sexp failed-packages))
             (scenario (concat-interpose-newline failed-ls)))
        (spit "~/.emacs.d/install-scenario.el" scenario))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\(use-package-with-report\\)" . font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;; global-safe-set-key : 安全なglobalsetkeyとエラーレポート、キーバインドレポート
;;; ---------------------------------------------------------------------------

;; キーバインド情報用(標準以外)
(defvar gssk-keybind-report '())

(defvar gssk-current-category-state "")

(defvar gssk-current-subcategory-state "")

(defvar gssk-current-function-name-state "")

(defun gssk-category
  (text)
  (setq gssk-current-category-state text))

(defun gssk-subcategory
  (text)
  (setq gssk-current-subcategory-state text))

(defun gssk-explain-function
  (text)
  (setq gssk-current-function-name-state text))

(defun gssk-category-function
  (category-text subcategory-text function-text)
  (setq gssk-current-category-state category-text)
  (setq gssk-current-subcategory-state subcategory-text)
  (setq gssk-current-function-name-state function-text))

(defvar gsskey-report-text nil)

(defun gssk-add-keybind-report
 (keybind-str sym)
 (add-to-list
  'gssk-keybind-report
  (list gssk-current-category-state
		gssk-current-subcategory-state
		keybind-str (symbol-name sym)
		gssk-current-function-name-state)))

(defmacro gssk-bind (keybind-str sym)
  `(cond
    ((fboundp ,sym)
	 (progn
	   (gssk-add-keybind-report ,keybind-str ,sym)
	   (global-set-key (kbd ,keybind-str) ,sym)))
    (t
     (setq gsskey-report-text
	   (concat gsskey-report-text
               "  - failed to bind: "
               (symbol-name ,sym)
               "\n")))))

(defun report-gsskey ()
  (if (not gsskey-report-text)
      "all key-bindings defined successfully"
    (concat " gsskey error: \n" gsskey-report-text)))

;;; ---------------------------------------------------------------------------
;;; gssk : binding report
;;; ---------------------------------------------------------------------------
(defconst grm-keybind-header
  "|分類1|分類2|キー|関数名|内容|")

(defconst grm-keybind-table-line
  (concat
   "| -------- |:----|:-------- "
   "| -------------------- |:-------|"))

(defun gssk-setting-md "")

(defun generate-explanation-text ()
  (apply 'concat
		 (mapcar '(lambda (x) (concat "|" (car x)
									  "|" (car (cdr x))
									  "|" (car (cdr (cdr x)))
									  "|" (car (cdr (cdr (cdr x))))
									  "|" (car (cdr (cdr (cdr (cdr x))))) "|\n"))
				 (reverse gssk-keybind-report))))

(defun key-binding-md ()
  (concat-interpose-newline
   (list grm-keybind-header
         grm-keybind-table-line
         (generate-explanation-text))))

;;; ---------------------------------------------------------------------------
;;; configuration report
;;; ---------------------------------------------------------------------------
(defun report-configuration ()
  (insert
   (concat
    ;; fortune message
    (ignore-errors
      (shell-command-to-string "fortune | rev | cowsay -f ghostbusters" ))
    (comment-out-message
     (concat-interpose-newline
      (list
       (concat-interpose-newline
        '("hello world, emacs !!" "('･_･`) ↓" "reports in loading init.el"))
       (report-failed-packages)
       (report-gsskey)))))))

;;; ---------------------------------------------------------------------------
;;; generate readme
;;; ---------------------------------------------------------------------------
(defconst readme-file-md "~/.emacs.d/README.md")

(defun generate-readme-text ()
  (concat
   (slurp "~/.emacs.d/datafiles/readme-template.rm")
   ;; explain key binds
   "\n## キーバインド\n\n"
   "デフォルト以外のglobal-set-key設定\n\n"
   (key-binding-md)))

;;; --------------------------------------------------------------------------------
;;; provide
;;; --------------------------------------------------------------------------------
(provide 'config-utils)
;;; config-utils.el ends here
