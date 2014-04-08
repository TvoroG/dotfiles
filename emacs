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
               python-mode)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)

;;;; Themes
(load-theme 'misterioso)

;;;; Python
;; M-x jedi:install-server RET
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; autopair
(setq autopair-global-mode t)
;; flycheck, pylint, hlint
(add-hook 'python-mode-hook 'flycheck-mode)
;; check manually: C-c ! c
(setq flycheck-check-syntax-automatically '())
