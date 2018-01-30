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

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

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
; search header/source files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-get-other-file)))

; ditto, and search #include
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c i") 'ff-find-other-file)))

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

; ;;;;;;;;;;;; Ros emacs ;;;;;;;;;;;;;;;
; ;; Load the library and start it up
; (require 'rosemacs)
; (invoke-rosemacs)

; ;; Optional but highly recommended: add a prefix for quick access
; ;; to the rosemacs commands
; (global-set-key "\C-x\C-r" ros-keymap)


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
                     cmake-mode
                     jedi 
                     yasnippet
                     yasnippet-snippets
                     neotree 
                     cygwin-mount
                     markdown-mode
                     tabbar
                     auctex
                     monokai-theme
                     clang-format
                     protobuf-mode
                     qml-mode
                     google-c-style))

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

;;;; Markdown files ;;;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;; tarbar ;;;;;;;;;;;;;;;
(require 'tabbar)
(tabbar-mode t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(verilog-case-indent 0)
 '(verilog-cexp-indent 0)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 4))


;;;;; AucTeX ;;;;;
;; start server, so synctex can work
(add-hook 'LaTeX-mode-hook 'server-start)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)


;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("make" "make" TeX-run-TeX nil t
      :help "Runs make")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))


;;;; UNCOMMENT for OSX ;;;;;

;; ;; use Skim as default pdf viewer
;; ;; Skim's displayline is used for forward search (from .tex to .pdf)
;; ;; option -b highlights the current line; option -g opens Skim in the background  
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;;;; UNCOMMENT for Windows ;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Forward/Inverse Search
;; ;;   - Need to compile with -synctex=1 option (see .latexmkrc file)
;; ;;   - SumatraPDF: use your install path
;; ;;   - Reference: sumatrapdf command line arguments wiki
;; ;;   - Inverse search requires setup in SumatraPDF
;; ;;      "C:\Program Files (x86)\GNU Emacs 24.5\bin\emacsclientw.exe" -n +%l "%f"
;; (setq TeX-view-program-list 
;;  '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance -forward-search %b %n %o"))))

;; (setq TeX-view-program-selection
;;       '((output-pdf "Sumatra PDF")))

;;;;; Coding Style ;;;;;
(global-set-key (kbd "C-c C-f") (lambda () (interactive) (clang-format-buffer "Google")))


;; protobuf mode
(require 'protobuf-mode)
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))
(add-to-list 'auto-mode-alist '("\\.pb.ascii\\'" . protobuf-mode))


;;; Qt QML ;;;
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

