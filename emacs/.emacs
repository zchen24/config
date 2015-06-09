;;;;;;;;; Environment Setup ;;;;;;
;; Outline
;; - Basic setup
;; - Packages
;;   - tabbar
;;   - cmake 
;;   - neotree
;;   - YASnippet
;;   - Backup (not currently used)
;;   - color theme
;;   - cygwin mount

;;;;;;;;;;;; Emacs Basic Setting ;;;;;;;;

;; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)


;; hightlight current line
(require 'hl-line)
;(global-hl-line-mode t)

;;;;;;;; cursor ;;;;;;;;;;;;;;;;;
(setq-default cursor-type 'bar)
;;to set the cursor color
(set-cursor-color "red")
;; move mouse if it's close to the cursor
(mouse-avoidance-mode 'animate)


;;;;;;;;;; Spell Checking ;;;;;;;;;
(setq-default ispell-program-name "aspell")



;; start text-mode by default
(setq default-major-mode 'text-mode)

;;;;;; column & line number ;;;;;;;
;; show column number
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode t)

;;;;; Scroll 1 line at a time
(setq scroll-step            1
      scroll-conservatively  10000)


;;;;;; Emacs Window Size ;;;;;;;;;;
(setq default-frame-alist 
      '((height . 50) (width . 100)
        (top . 1) (left . 800)
        ))


;;;;;;; Disable Welcome ;;;;;;;;;;;;;
(setq inhibit-startup-message t)

;;;;;;;;;;;;; Time ;;;;;;;;;;;;;;;;;
;; display time
(display-time-mode 1)

;; set time display format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;; delete scroll bar
(scroll-bar-mode 1)
(tool-bar-mode -1)

;; set auto column change
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(setq default-fill-column 80)

;;;;;;;;;; Uncomment ;;;;;;;;;;;;;;;
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-y" 'uncomment-region)
(fset 'cmr 'comment-region)

;;;;;;;;;;; Shortcut Keys ;;;;;;;;;;
(global-set-key [f5] 'compile)
(global-set-key [f8] 'calendar)
(global-set-key [f9] 'list-bookmarks)
(global-set-key [f10] 'speedbar)
;(global-set-key [f11] 'todo-mode)

;;; Recent opened file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-c\ \C-r" 'recentf-open-files)
(fset 'rof 'recentf-open-files)


;;;;;;;;;;; Autopair (){}[] ;;;;;;;;;

(defun my-c-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist) 
  (setq skeleton-pair-alist  '(
    (?` ?` _ "''") 
    (?\( ?  _ ")") 
    (?\" ?  _ "\"")
    (?\[ ?  _ "]") 
    (?{ \n > _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
;; (add-hook 'c-mode-hook 'my-c-mode-auto-pair)
;; (add-hook 'c++-mode-hook 'my-c-mode-auto-pair)





;;;;;;;;; Auto  Complementation ;;;;;;;
;; M-/ for hippie expand 
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
	  '(try-expand-dabbrev
		try-expand-dabbrev-visible
		try-expand-dabbrev-all-buffers
		try-expand-dabbrev-from-kill
		try-complete-file-name-partially
		try-complete-file-name
		try-expand-all-abbrevs
		try-expand-list
		try-expand-line
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol
        try-expand-whole-kill
        )
      )



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


;;;;;;; Copy n Line ;;;;;;;;;;;;;;;;;
;; C-c C-w copy 1 line, C-u 5 C-c C-w copy 5 lines
(global-set-key (kbd "C-c C-w") 'copy-lines)
(defun copy-lines(&optional arg)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (next-line arg)
    (kill-ring-save (mark) (point))
    )
  )



;;;;;;;; Move line up/down ;;;;;;;;;;;
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down) 
(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
     (setq n 1))
  (let ((col (current-column)))
     (beginning-of-line)
     (next-line 1)
     (transpose-lines n)
     (previous-line 1)
     (forward-char col)))
(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n)))) 

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))



;;;;;; New line & indent ;;;;;;;;;;;;;
;(global-set-key [(control o)] 'vi-open-next-line)

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  PLUGIN  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/pkgs/tabbar")
;; (require 'tabbar)
;; (tabbar-mode t)
;; (global-set-key (kbd "<C-tab>") 'tabbar-forward)
;; (global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)
;; (global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
;; (global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/cmake-mode")
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-view-program-list
      (quote (("Okular" "okular -unique %o#src:%n%b"))))

(setq TeX-view-program-selection
      `(((output-dvi style-pstricks) "dvips and gv")
        (output-dvi "Okular")
        (output-pdf "Okular")
        (output-html "xdg-open")))

(defun my-tex-mode-hook()
  "my tex-mode hook"
  (tex-pdf-mode))

(setq TeX-auto-save t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'my-tex-mode-hook)
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)


;;; doxymacs support
;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;; (setq doxymacs-doxygen-style "Qt")
;; (setq user-mail-address "zihan.chen@jhu.edu")  ;; default email



;;; c-perl mode 
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATLAB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/matlab-mode")
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")




(show-paren-mode 1)




;;; For mixed dos/unix style CF ;;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/c++ header/source toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq ff-search-directories
      '("." "../src" "../../src" "../include"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Packages Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tutorial:
;; http://www.youtube.com/watch?v=rGVVnDxwJYE
(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete")
(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete-c-headers")
(add-to-list 'load-path "~/.emacs.d/pkgs/popup")
(add-to-list 'load-path "~/.emacs.d/pkgs/iedit")

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; ;;; ac c headers
;; (defun my:ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers))
;; (add-hook 'c++-mode-hook 'my:ac-c-header-init)
;; (add-hook 'c-mode-hook 'my:ac-c-header-init)

;; key-binding
(define-key ac-mode-map (kbd "M-/") 'auto-complete )
(setq global-auto-complete-mode t)



;; ;; ;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;; Flymake ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/flymake-easy")
(add-to-list 'load-path "~/.emacs.d/pkgs/flymake-cursor")
(add-to-list 'load-path "~/.emacs.d/pkgs/flymake-google-cpplint")

(defun my:flymake-google-cpplint-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables '(flymake-google-cpplint-command
                          "c:\cygwin\bin\cpplint"))
  (flymake-google-cpplint-load))
(add-hook 'c++-mode-hook 'my:flymake-google-cpplint-init)
(add-hook 'c-mode-hook 'my:flymake-google-cpplint-init)


;;;;;;;;;;;;;;;;; Flymake ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;;;;;;;;;;;;;;;; CEDET ;;;;;;;;;;;;;;;;;;;;;;
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)


;;;;;;;;;;;; color-theme ;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/color-theme")
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)


;;;;;;;;;;;; cygwin-mount ;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/pkgs/cygwin-mount")
(setq *win32* (eq system-type 'windows-nt) )
;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
(if *win32*
    (progn
      (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
      (require 'setup-cygwin)
    ;(setenv "HOME" "c:/cygwin/home/someuser") ;; better to set HOME env in GUI
      ))



; Zihan 2012-07-07
; Note: will not use
;;;;;;;;;;; Todo ;;;;;;;;;;;;;;;;;;;;
;(setq todo-file-do "~/.emacs.d/todo-do")
;(setq todo-file-done "~/.emacs.d/todo-done")
;(setq todo-file-top "~/.emacs.d/todo-top")
;;;;;;;;;;; Diary ;;;;;;;;;;;;;;;;;;;;;
;(setq diary-file "~/.emacs.d/diary")
;(setq diary-
;;;;;;;;;;; Appointment ;;;;;;;;;;;;;;;;
;(setq appt-issue-message t)
