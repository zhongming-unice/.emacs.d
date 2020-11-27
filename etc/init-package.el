;;; init-package --- initialize the plugins

;;; Commentary:


;;; Code:

;; ******************** PART1 benchmark(Optional) ********************
;; Settings for benchmark package
;; (use-package benchmark-init
;;   :init (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))




;; ******************** PART2 shell & environments ********************
;; Settings for org mode and load config from org file
(use-package org
  ;; :init (setq org-startup-indented t)
  :bind (("C-c s" . org-insert-structure-template))
  :config
  (setq org-startup-indented t
	org-todo-keywords '((sequence "TODO" "DOING" "DONE"))
	org-todo-keyword-faces '(("DOING" . "blue")))
  (use-package org-bullets
    :when (display-graphic-p)
    :hook (org-mode . (lambda() (org-bullets-mode 1)))))


(add-hook 'org-mode-hook
	  (lambda()
	    (setq truncate-lines nil))) 

(setq org-html-xml-declaration (quote (("html" . "")
                                       ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                       ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

(setq org-html-validation-link nil)

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; 竖线
(use-package 
  page-break-lines 
  :ensure t 
  :config (turn-on-page-break-lines-mode))

(use-package vterm
    :ensure t)

(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp--client	
  ;; 	:new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
  ;; 	:major-modes '(c-mode c++-mode)
  ;; 	:server-id 'ccls-remote
  ;; 	:multi-root nil
  ;; 	:remote? t
  ;; 	;; :notification-handlers
  ;; 	;; (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
  ;; 	;; 		("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
  ;; 	:initialization-options (lambda () ccls-initialization-options)
  ;; 	:library-folders-fn nil))
  )

(use-package modern-cpp-font-lock
  :ensure t
  :diminish t
  :init (modern-c++-font-lock-global-mode t))

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; ******************** PART3 editing ********************
;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
(defadvice gui-backend-get-selection (around get-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((default-directory "~/"))
      (setq ad-return-value (shell-command-to-string "pbpaste")))))

;; "+yy 设置内容到系统clipboard
;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  ;; (message "%s %s"  (ad-get-arg 0)  (ad-get-arg 1))
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil)   ; ; use pipe
          (default-directory "~/"))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (ad-get-arg 1))
        (process-send-eof proc)))))
;; linux 粘贴板
(defun my-pclip (str-val)
  (if simpleclip-works (simpleclip-set-contents str-val)
    (cond
     ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input"))))))

;; Settings for C-a behavior
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c ^" . crux-top-join-line)
	     ("C-x ," . crux-find-user-init-file)
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ("C-S-k" . crux-smart-kill-line)))

;; Hungry Delete - delete multi spaces with one <delete> key
(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward)))

;; drag-stuff - move lines up/down
(use-package drag-stuff
  :bind (("ESC <up>". drag-stuff-up)
	 ("ESC <down>". drag-stuff-down)
	 ("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;; Settings for company
(use-package company
  ;; The next line cause lsp-mode bugs when complete functions
  ;; (push '(company-semantic :with company-yasnippet) company-backends)
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (prog-mode . company-mode)
  :config (setq company-dabbrev-code-everywhere t
		company-dabbrev-code-modes t
		company-dabbrev-code-other-buffers 'all
		company-dabbrev-downcase nil
		company-dabbrev-ignore-case t
		company-dabbrev-other-buffers 'all
		company-require-match nil
		company-minimum-prefix-length 2
		company-show-numbers t
		company-tooltip-limit 20
		company-idle-delay 0
		company-echo-delay 0
		company-tooltip-offset-display 'scrollbar
		company-begin-commands '(self-insert-command))
  :bind (:map company-active-map
             ("M-n" . nil) 
             ("M-p" . nil) 
             ("C-n" . #'company-select-next) 
             ("C-p" . #'company-select-previous)))

(use-package company-quickhelp
  :hook (prog-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.3))

;; 人工智能补全代码
(use-package 
  company-tabnine
  :disabled 
  :ensure t 
  :after 'company-mode 
  'company-tabnine-mode 
  :config (add-to-list 'company-backends #'company-tabnine))

;; awesome-pair 
(use-package 
  awesome-pair 
  :disabled
  :config (dolist (hook (list 'c-mode-common-hook 'c-mode-hook 'c++-mode-hook 'java-mode-hook
                              'haskell-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
                              'lisp-mode-hook 'maxima-mode-hook 'ielm-mode-hook 'sh-mode-hook
                              'makefile-gmake-mode-hook 'php-mode-hook 'python-mode-hook
                              'js-mode-hook 'go-mode-hook 'qml-mode-hook 'jade-mode-hook
                              'css-mode-hook 'ruby-mode-hook 'coffee-mode-hook 'rust-mode-hook
                              'qmake-mode-hook 'lua-mode-hook 'swift-mode-hook
                              'minibuffer-inactive-mode-hook)) 
            (add-hook hook '(lambda () 
                              (awesome-pair-mode 1)))) 
  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (defpine-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline))


;; electric-pair
(electric-pair-mode 1)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))
;; ******************** PART4 searching ********************
;; Settings for ivy & counsel & swiper
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (use-package ivy-posframe
    :unless *is-windows*
    :when (display-grayscale-p)
    :init
    (setq ivy-posframe-display-functions-alist
          '((swiper            . ivy-posframe-display-at-frame-center)
            (complete-symbol   . ivy-posframe-display-at-point)
            (counsel-M-x       . ivy-posframe-display-at-frame-center)
            (counsel-find-file . ivy-posframe-display-at-frame-center)
            (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
            (t                 . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-mode 1)))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ("C-h b" . counsel-descbinds)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))




;; ******************** PART5 basic development ********************
;; Settings for which-key - suggest next key
;; (use-package which-key
;;   :defer nil
;;   :config (which-key-mode))

;; Settings for magit
;; I quit using magit on windows, 'cause its performance sucks
;; I use emacs builtin vc & cli-git on windows instead
(use-package magit
  :unless *is-windows*
  :bind ("C-x g" . magit-status))

;; Settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat
				  (file-name-directory user-emacs-directory)
				  "snippets"))
  (use-package yasnippet-snippets
    :after yasnippet)
  (use-package auto-yasnippet
    :bind (("C-o" . aya-open-line)
           ("H-w" . aya-create)
           ("H-y" . aya-expand))))

;; Settings for projectile
;; Using after-init hook makes emacs starts up faster than config projectile-mode
(use-package projectile
  :hook (after-init . projectile-mode)
  ;; :config (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Enable flymake on default
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; Settings for highlight parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Settings for geiser
(setq scheme-program-name "chez")
(setq geiser-chez-binary "chez")
(setq geiser-active-implementations '(chez))

;; ******************** PART6 Emacs Optimize ********************
;; Settings for jump windows, use M-NUM to switch
(use-package ace-window
  :bind (("M-o" . 'ace-window)))

;; Restart emacs
(use-package restart-emacs)

;; Google translate
;; (use-package google-translate
;;   :init (setq google-translate--tkk-url "https://translate.google.cn"
;;               google-translate-default-source-language "en"
;;               google-translate-default-target-language "zh-CN")
;;   :bind (("C-c t" . google-translate-at-point)
;;          ("C-c T" . google-translate-query-translate)))

;; (use-package 'go-translate
;;   :init (setq go-translate-base-url "https://translate.google.cn"
;; 	      go-translate-local-language "zh-CN")
;;   :bind (("C-c t" . go-translate)
;;          ("C-c T" . go-translate-popup)))

(setq go-translate-local-language "zh-CN")
(global-set-key "\C-ct" 'go-translate)
(global-set-key "\C-cc" 'go-translate-popup)
(setq go-translate-base-url "https://translate.google.cn")

(use-package keycast
  :commands keycast-mode)

(provide 'init-package)
;;; init-package.el ends here
