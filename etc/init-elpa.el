;;; init-elpa --- initialize elpa repository

;;; Commentary:
;;;  

;;; Code:

;;; Settings for package archives
;; (setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                          ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))		  

(setq package-check-signature nil)

(require 'package)

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  ;; (when (version< emacs-version "27.0")
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-always-demand nil
        use-package-expand-minimally t
        use-package-verbose t))
(setq load-prefer-newer t)

(eval-and-compile
  (require 'use-package))

(provide 'init-elpa)
;;; init-elpa.el ends here
