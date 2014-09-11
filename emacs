;;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-recipe-path '("~/.emacs.d/el-get/el-get/recipes"))
(setq el-get-verbose t)

;; personal recipes
(setq el-get-sources
      '((:name better-defualts :type github :pkgname "technomancy/better-defaults")
        (:name moe-theme :type github :pkgname "kuanyui/moe-theme.el")))

(setq my-packages
      (append
       ;; list of packages from official recipes
       '(magit js2-mode haskell-mode color-theme color-theme-sanityinc
               color-theme-almost-monokai jedi autopair flycheck
               python-mode rust-mode edbi git-gutter helm web-mode ag
               projectile yasnippet fill-column-indicator tern powerline
               virtualenvwrapper color-theme-solarized auto-complete)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)

;;;; customization
(setq default-input-method "russian-computer")
;;(set-frame-font "Inconsolata lgc 10" t t)
;;(set-frame-font "Consolas 11" t t)
(set-frame-font "Source Code Pro light 10" t t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'misterioso)
;;(load-theme 'moe-dark t)
(require 'moe-theme)
(moe-dark)
(powerline-moe-theme)

;;;; Python
;; M-x jedi:install-server RET
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun jedi-config:setup-keys ()
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

;; flycheck, pylint, hlint
(add-hook 'python-mode-hook 'flycheck-mode)
;; check manually: C-c ! c
(setq flycheck-check-syntax-automatically '())

;; virtualenvwrapper
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs")
;;(setenv "PYTHONPATH" "path")
(add-hook 'python-mode-hook (lambda () (fci-mode t)))


;;;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (progn
              (fci-mode t)
              (tern-mode t)
              (setq js2-basic-offset 2))))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(setq tern-ac-on-dot t)


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
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c C-f") 'helm-find-files)

;;;; windows
(defun split-window3()
  (interactive)
  (progn (delete-other-windows)
         (split-window-horizontally)
         (split-window-vertically)
         (other-window 2)))

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)

;;;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;;;; projectile
;;(add-hook 'python-mode-hook 'projectile-on)
;;(add-hook 'js2-mode-hook 'projectile-on)
(projectile-global-mode)

;;;; yasnippet
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-c C-e") 'yas-expand)

;;;; fci
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode t)
(setq fci-rule-column 100)

;;;; auto-complete
(global-auto-complete-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
