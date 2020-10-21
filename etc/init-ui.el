;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:


;;; Code:


;; Settings for UI theme
;;(use-package gruvbox-theme
;;  :unless *is-windows*
;;  :when (display-graphic-p)
;;  :init (load-theme 'gruvbox-dark-soft t))
;; (use-package smart-mode-line-powerline-theme)

;; Settings for UI theme
;; (use-package solarized-theme
;;   :unless *is-windows*
;;   :when (display-graphic-p)
;;   :init (load-theme 'solarized-dark t))



(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	    sml/theme 'respectful)
  ;; (if *is-windows*
  ;; (setq sml/theme 'powerline
  ;; sml/theme 'powerline))
  (sml/setup))

;; Font settings
(use-package emacs
  :when (display-graphic-p)
  :config
  (set-default 'cursor-type 'bar)
  (setq default-frame-alist '((width . 150) (height . 35))))
;; 窗口最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(split-window-right)
;; 彩虹括号
;; (use-package rainbow-delimiters 
;;   :ensure t 
;;   :config
;;   ;; 设置每一级括号的颜色
;;   (set-face-foreground 'rainbow-delimiters-depth-1-face "orange red") 
;;   (set-face-foreground 'rainbow-delimiters-depth-2-face "gold") 
;;   (set-face-foreground 'rainbow-delimiters-depth-3-face "yellow") 
;;   (set-face-foreground 'rainbow-delimiters-depth-4-face "spring green") 
;;   (set-face-foreground 'rainbow-delimiters-depth-5-face "cyan") 
;;   (set-face-foreground 'rainbow-delimiters-depth-6-face "magenta") 
;;   (set-face-foreground 'rainbow-delimiters-depth-7-face "goldenrod") 
;;   (set-face-foreground 'rainbow-delimiters-depth-8-face "IndianRed1") 
;;   (set-face-foreground 'rainbow-delimiters-depth-9-face "ivory1") 
;;   (set-face-bold 'rainbow-delimiters-depth-1-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-2-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-3-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-4-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-5-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-6-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-7-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-8-face "t") 
;;   (set-face-bold 'rainbow-delimiters-depth-9-face "t") 
;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; 启动界面
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; (require 'recentf) ;; Provided for the whole picture
;; (require 'helm)
;; (require 'helm-config)
;; (if (< (length command-line-args) 2) 
;; (setq initial-buffer-choice (car (helm-recentf)))
;; )

(use-package 
  dashboard 
  :ensure t 
  :config (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text") 
                                    (bookmarks . "book")))
  ;; 设置标题
  (setq dashboard-banner-logo-title
        "糊纸袋的emacs")
  ;; 设置banner
;;   (setq dashboard-startup-banner "~/.emacs/") 
  (setq dashboard-center-content t) 
  (setq dashboard-set-heading-icons t) 
  ;; (setq dashboard-set-file-icons t) 
  (setq dashboard-set-navigator t))


(require 'recentf)
(recentf-mode t)

;; Hide scroll bar and tool bar in GUI mode
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)


(provide 'init-ui)
;;; init-ui.el ends here
