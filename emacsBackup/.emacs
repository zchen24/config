;;;;;;;;; Environment Setup ;;;;;;

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


;;;;;; Emacs Window Size ;;;;;;;;;;
(setq default-frame-alist 
      '((height . 50) (width . 100)
        ;; (top . 1) (left . 1)
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
(scroll-bar-mode nil)
(tool-bar-mode nil)

;; set auto column change
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(setq default-fill-column 80)



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





;;;;;;;;;; CEDET Setup ;;;;;;;;;;;;;
;(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;(global-ede-mode 1)   ; Enable proj management sys
;(semantic-load-enable-code-helpers) ;prototype help smart completion
;(global-srecode-minor-mode 1)  ;template insertion menu     
;(require 'semantic-ia)





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
;(add-hook 'c-mode-hook 'my-c-mode-auto-pair)
;(add-hook 'c++-mode-hook 'my-c-mode-auto-pair)





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

;;;;;;;;;;;; tarbar.el ;;;;;;;;;;;;;;;
;; tarbar.el plugin path
(add-to-list 'load-path "~/.emacs.d/")
;; open tabbar mode
(require 'tabbar)
(tabbar-mode t)
;; customized code for buffer groups)

;; setup related shortcut tab
(global-set-key (kbd "<C-tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)
;; tabbar group
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)



;;;;;;;;;;;; color-theme.el ;;;;;;;;;;;;
;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)


;;;;;;;;;;;;; autopair.el ;;;;;;;;;;;;;;
;(require 'autopair)
;(autopair-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  mew-emacs setup  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))


;;;;;;; Add Support for CMakeLists.txt;;;;
;;;;;;;;;;;; Cmake-mode.el ;;;;;;;;;;;;;;;
(require 'cmake-mode)
(setq auto-mode-alist
       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                 ("\\.cmake\\'" . cmake-mode))
               auto-mode-alist))


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






;;; For Tex-View use Okular
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


;;; doxymacs support
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "Qt")
(setq user-mail-address "zihan.chen@jhu.edu")  ;; default email


;;; Recent opened file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-c\ \C-r" 'recentf-open-files)
(fset 'rof 'recentf-open-files)


;;; c-perl mode 
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))




;;;;;;;;;;;;;; Auto-complete ;;;;;;;;;;;;;;;

;;; auto-complete mode
; Tutorial:
; http://www.youtube.com/watch?v=rGVVnDxwJYE
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
; key-binding
;(define-key ac-mode-map (kbd "M-/") 'auto-complete )

(require 'auto-complete)
(setq global-auto-complete-mode t)



;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;

(autoload 'python-mode "python-mode" "Python Mode." t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;;; ropemacs mode
(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c C-p")
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)



;; ;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ; personal snippet
;; (setq yas/root-directory "~/.emacs.d/snippets")
;; (yas/load-directory "~/.emacs.d/snippets")
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

(add-to-list 'load-path "~/.emacs.d/plugins/jade")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


(setq scroll-step            1
      scroll-conservatively  10000)



;;;;;;;;;;;;;; MATLAB ;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;; Ros emacs ;;;;;;;;;;;;;;;
;; Load the library and start it up
(require 'rosemacs)
(invoke-rosemacs)

;; Optional but highly recommended: add a prefix for quick access
;; to the rosemacs commands
(global-set-key "\C-x\C-r" ros-keymap)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rng-schema-locating-files
   (quote ("/usr/share/emacs/site-lisp/rosemacs-el//rng-schemas.xml"
           "schemas.xml"
           "/usr/share/emacs/23.3/etc/schema/schemas.xml"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;;; switch between header/source file ;;;;;
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq ff-search-directories
      '("." "../src" "../../src" "../include"))



;;;; Markdown files ;;;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; Use google-chrome ;;;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;;; JavaScripte ;;;;
(setq js-indent-level 2)
;;;; JSON ;;;;
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))


;;;; Google Protobuf ;;;
(add-to-list 'load-path "~/.emacs.d/plugins/protobuf")
(require 'protobuf-mode)
(setq auto-mode-alist  (cons '(".proto$" . protobuf-mode) auto-mode-alist))


;;;; nesC mode ;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/nesc")
(setq load-path (cons (expand-file-name "X") load-path))
(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))

;;;; smart indent ;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/smarttab")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/CC Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ellemtel kernel style 
(setq c-default-style "ellemtel")

;; #if 0 change color
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'nesc-mode-hook 'my-c-mode-common-hook)