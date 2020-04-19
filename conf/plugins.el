;; Install all packages
(dolist (package '(
					;; javascript packages
		   js2-mode		;; https://github.com/mooz/js2-mode
		   js2-refactor		;; https://github.com/magnars/js2-refactor.el
		   xref-js2
		   rainbow-mode         ;; Colorize color names in buffers
		   )
	)
 (unless (package-installed-p package)
   (package-install package))
 (require package))


;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d ") ;; задаем формат нумерации строк

;; IDO plugin
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(setq ido-everywhere           t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages (quote (json-mode sr-speedbar))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
;; SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

;; Modes
;; nXML mode
(setq nxml-child-indent 4 nxml-attribute-indent 4)
(setq nxml-slash-auto-complete-flag t)


(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
