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

;;;; switch between header/source file ;;;;;
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq ff-search-directories
      '("." "../src" "../../src" "../include"))

;;;;;;; Tab realted setting;;;;;;;;;;
(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

(defun set-4offset(&optional arg)
  (interactive "p")
  (setq default-tab-width 4)
  (setq c-basic-offset 4)
  )

(defun set-2offset(&optional arg)
  (interactive "p")
  (setq default-tab-width 2)
  (setq c-basic-offset 2)
  )

;;;;;;; set title "title@Emacs";;
(setq frame-title-format "%b@Emacs")

;;;;;;;; cursor ;;;;;;;;;;;;;;;;;
;; (setq-default cursor-type 'bar)
;;to set the cursor color
;; (set-cursor-color "red")
;; move mouse if it's close to the cursor
(mouse-avoidance-mode 'animate)


;;;;;;;;;;; Shortcut Keys ;;;;;;;;;;
(global-set-key [f9] 'list-bookmarks)

;;;;; window management ;;;;;;
(global-set-key (kbd "C-c <up>")   'shrink-window)                                           
(global-set-key (kbd "C-c <down>") 'enlarge-window)                                        
(global-set-key (kbd "C-c <left>")  'enlarge-window-horizontally)                           
(global-set-key (kbd "C-c <right>") 'shrink-window-horizontally)     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Packages Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; list the packages you want
(setq package-list '(matlab-mode 
                     color-theme 
                     python-mode
                     jedi 
                     yasnippet 
                     neotree 
                     cygwin-mount 
                     monokai-theme))

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
;; (color-theme-initialize)
;; (color-theme-calm-forest)
;; (load-theme 'monokai t)
(require 'monokai-theme)

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



;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;
;; python mode based on pymacs & ropemacs 
;; compared with jade, it provides better refactoring feature
;; this snippet is listed here as a backup option 

;; (autoload 'python-mode "python-mode" "Python Mode." t)

;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (require 'python-mode)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;;; ropemacs mode
;; (setq ropemacs-enable-shortcuts nil)
;; (setq ropemacs-local-prefix "C-c C-p")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)


;;;;;; CMake ;;;;;;;;;;;;
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))


;; ;;;;;; cygwin ;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/pkgs")
;; (setq *win32* (eq system-type 'windows-nt) )
;; ;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
;; (if *win32*
;;     (progn
;;       (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
;;       (require 'setup-cygwin)
;;     ;(setenv "HOME" "c:/cygwin/home/someuser") ;; better to set HOME env in GUI
;;       ))

;;;;; chines input ;;;;;;
;; (require 'chinese-pyim)
;; (setq-default pyim-page-length 9)
;; (setq default-input-method "chinese-pyim")
