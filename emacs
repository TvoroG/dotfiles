;;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-recipe-path '("~/.emacs.d/el-get/el-get/recipes"))
(setq el-get-verbose t)

;; personal recipes
(setq el-get-sources
      '((:name better-defualts
	       :type github :pkgname "technomancy/better-defaults")))

(setq my-packages
      (append
       ;; list of packages from official recipes
       '(magit js2-mode haskell-mode color-theme color-theme-sanityinc
               color-theme-almost-monokai jedi autopair flycheck
               python-mode rust-mode edbi git-gutter nxhtml helm)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)

;;;; Themes
;;(load-theme 'misterioso)

;;;; Python
;; M-x jedi:install-server RET
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; flycheck, pylint, hlint
(add-hook 'python-mode-hook 'flycheck-mode)
;; check manually: C-c ! c
(setq flycheck-check-syntax-automatically '())
;; Django
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;;;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; autopair
(autopair-global-mode)
;;;; git
(global-git-gutter-mode +1)
;;;; nxhtml
(setq mumamo-background-colors nil)
;; to disable mumamo warnings
(eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars)))

;;;; helm
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c C-f") 'helm-find-files)
