;;; init-lsp --- lsp settings

;;; Commentary:

;;; CODE:

;; (use-package lsp-mode
;;   ;; add prog-mode to lsp instead of adding one by one
;;   ;; :hook ((prog-mode . (lsp-deferred))
;; 	 ;; (lsp-mode . lsp-enable-which-key-integration))
;;   :hook ((lsp-mode . lsp-enable-which-key-integration)
;; 	 (python-mode . lsp-deferred)
;;          (c-mode . lsp-deferred)
;;          (go-mode . lsp-deferred)
;;          (java-mode . lsp-deferred)
;;          (js-mode . lsp-deferred)
;;          (web-mode . lsp-deferred)
;;          (vue-mode . lsp-deferred)
;;          (html-mode . lsp-deferred))
;;   :commands (lsp lsp-deferred)
;;   :init (setq lsp-keep-workspace-alive nil ;; Auto kill LSP server
;;               lsp-enable-indentation nil
;;               lsp-enable-on-type-formatting nil
;;               lsp-auto-guess-root nil
;;               lsp-enable-snippet t)
;;   :config
;;   ;; Configure LSP Clients
;;   (use-package lsp-clients
;;     :ensure nil
;;     :functions (lsp-format-buffer lsp-organize-imports)))

;; ;;; Optionally: lsp-ui, company-lsp
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :init (setq lsp-ui-doc-enable t
;;               lsp-ui-doc-use-webkit nil
;;               lsp-ui-doc-delay 0
;;               lsp-ui-doc-include-signature t
;;               lsp-ui-doc-position 'at-point
;;               lsp-eldoc-enable-hover nil ;; Disable eldoc displays in minibuffer
;;               lsp-ui-sideline-enable t
;;               lsp-ui-sideline-show-hover nil
;;               lsp-ui-sideline-show-diagnostics nil
;;               lsp-ui-sideline-ignore-duplicate t)
;;   :config (setq lsp-ui-flycheck-enable t)
;;   :commands lsp-ui-mode)


;; (use-package company-lsp
;;   :after company lsp-mode
;;   :config (setq company-lsp-enable-snippet t)
;;   :init (push 'company-lsp company-backends))

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package dap-mode
;;   :diminish
;;   :hook ((lsp-mode . dap-mode)
;;          (dap-mode . dap-ui-mode)
;; 	 (dap-mode . dap-tooltip-mode)
;;          (python-mode . (lambda() (require 'dap-python)))
;;          (go-mode . (lambda() (require 'dap-go)))
;;          (java-mode . (lambda() (require 'dap-java)))))


;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; org 自动换行
(add-hook 'org-mode-hook (lambda () (setq toggle-truncate-lines t)))

;; add .cu, .lex, .inl, .h to c++mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . c++-mode))
;; add .txt to makefile mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . makefile-mode))

;; add .erl to erlang mode 
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;; add ggtags-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

;;;;======================================================== < for gdb >

;;;; set gdb multi-windows when open
(setq gdb-many-windows t)

;;;; customize the gdb multi-windows
(defadvice gdb-setup-windows (after my-setup-gdb-windows activate)
  "my gdb UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))  ;; code and output
        (win2 (split-window-below (/ (* (window-height) 3) 4)))  ;; stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4))))  ;; io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (select-window win0)
    ))


(provide 'init-lsp)
;;; init-lsp.el ends here
