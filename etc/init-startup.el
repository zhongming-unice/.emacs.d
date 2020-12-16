;;; init-startup.el --- Works when startup Emacs

;;; Commentary:

;;; Code:

;; Settings for system encoding
(prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))


;; Settings for backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Adjust garbage collection thresholds during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq-default frame-title-format '("%f"))

;; (setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "
;; ┌──────────────────┐
;; │zhong ming's emacs│
;; └──────────────────┘
                                                                      
\n\n")

;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?- "w")))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; set previous/next buffer shortcuts
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)

(provide 'init-startup)
;;; init-startup.el ends here


