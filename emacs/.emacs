;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline
;; - basic emacs config
;; - setup and init packages
;; - config packages installed via package.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Basic Emacs Configuration 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; no toolbar
(tool-bar-mode -1)

;; disable welcome (C-h C-a)
(setq inhibit-startup-message t)

;;;;; Scroll 1 line at a time
(setq scroll-step            1
      scroll-conservatively  10000)

;;;;;; column & line number ;;;;;;;
;; show column number
(setq column-number-mode t)
(setq line-number-mode t)
;; (global-linum-mode t)

;;;;;; Emacs Window Size ;;;;;;;;;;
(setq default-frame-alist 
      '((height . 50) (width . 100)
        (top . 1) (left . 800)
        ))

;;; Recent opened file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-c\ \C-r" 'recentf-open-files)
(fset 'rof 'recentf-open-files)


;;;;;; ido mode ;;;;;;
(ido-mode t)

;;;;;;;;;;;;; Time ;;;;;;;;;;;;;;;;;
;; display time
(display-time-mode 1)

;; set time display format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;;;;;;;;;; Uncomment ;;;;;;;;;;;;;;;
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-y" 'uncomment-region)
(fset 'cmr 'comment-region)

;; set auto column change
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(setq default-fill-column 80)

;;;;;;;;;;; Shortcut Keys ;;;;;;;;;;
(global-set-key [f9] 'list-bookmarks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Packages Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list the packages you want
(setq package-list '(matlab-mode color-theme jedi yasnippet neotree cygwin-mount))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Configure Pkgs Installed via Package.el 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; color-theme ;;;;;;
(color-theme-initialize)
;; (color-theme-calm-forest)

;;;;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;;;; Yasnippet ;;;;;
(require 'yasnippet)
(yas-global-mode 1)


;;;;;;; Python ;;;;;;;

;; python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; jedi python auto-complete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional



;;;;;; CMake ;;;;;;;;;;;;
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))


;;;;;; cygwin ;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs")
(setq *win32* (eq system-type 'windows-nt) )
;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
(if *win32*
    (progn
      (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
      (require 'setup-cygwin)
    ;(setenv "HOME" "c:/cygwin/home/someuser") ;; better to set HOME env in GUI
      ))
