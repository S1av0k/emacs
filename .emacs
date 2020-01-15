;; Checking the type of operating system
(package-initialize)

(defun system-is-mac ()
    (interactive)
    (string-equal system-type "darwin"))

(defun system-is-linux ()
    (interactive)
    (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
    (interactive)
    (string-equal system-type "windows-nt"))

;; Start a server communication subprocess
(unless (system-is-windows)
    (require 'server)
    (unless (server-running-p)
        (server-start)))

;; MS Windows path-variable
(when (system-is-windows)
    (setq win-init-path         "../home/.emacs.d")
    (setq win-init-ct-path      "../home/.emacs.d/plugins/color-theme")
    (setq win-init-ac-path      "../home/.emacs.d/plugins/auto-complete")
    (setq win-init-ac-dict-path "../home/.emacs.d/plugins/auto-complete/dict"))

;; Set default font in initial window and for any new window
(if (system-is-windows) ; Microsoft Windows
  (when (member "Courier New" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Courier New-13"))
    (add-to-list 'default-frame-alist '(font . "Courier New-13"))
    (set-frame-font "Courier New-13" nil t)))

 (if (system-is-linux) ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-14"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))))

 (if (system-is-mac) ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))

;; Melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

;; My name and e-mail adress
(setq user-full-name   "Vyacheslav Beketnov")
(setq user-mail-adress "beketnov.vm@gmail.com")

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

;; Disable GUI components
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(blink-cursor-mode -1) ;; курсор не мигает
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал
(setq indent-tabs-mode nil) ; always use spaces, not tabs, when indenting
(setq case-fold-search t) ; ignore case when searching
(setq require-final-newline nil) ; require final newlines in files when they are saved

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(setq display-time-format      "%H:%M:%S")
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

;; Line wrapping
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Start window size
(when (window-system)
  (set-frame-size (selected-frame) 80 40))

;; Custom key bindings
; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Revert buffers then files change
(global-auto-revert-mode 12)

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы  
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search results
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

;; IDO plugin
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)
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


; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)
