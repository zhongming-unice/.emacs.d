 
;;; init.el --- initialization file

;;; Commentary:
;;  This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration.
;;  Written by (c) ZHONG Ming.  2020-2021.

;;; Code:

;; (load-file (concat (file-name-directory user-emacs-directory)
;;                    "lisp/core/core-load-paths.el"))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "etc/")))
(add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes/")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defconst *is-mac* (eq system-type 'darwin)
  "Const for system check, macOS.")

(defconst *is-linux* (eq system-type 'gnu/linux)
  "Const for system check, GNU/Linux.")

(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  "Const for system check, Windows or DOS.")

(require 'init-startup)
(require 'init-elpa)
(require 'awesome-pair)
(require 'init-package)
(require 'init-ui)
(require 'init-lsp)
(require 'init-web)
(require 'go-translate)

(when (file-exists-p custom-file)
  (load-file custom-file)) 

;;; init.el ends here
(put 'scroll-left 'disabled nil)

