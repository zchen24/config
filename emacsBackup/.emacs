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



;;;;;;; set title "title@Emacs";;
(setq frame-title-format "%b@Emacs")


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
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-comidia)
;(color-theme-subdued)
;(color-theme-oswald)
;(color-theme-twilight)
;(color-theme-arjen)
;(color-theme-zenburn)
;(Color-theme-gnome)
;(color-theme-tango)


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


;;;;;;;;;;; Shortcut Keys ;;;;;;;;;;
(global-set-key [f5] 'compile)
(global-set-key [f8] 'calendar)
(global-set-key [f9] 'list-bookmarks)
;(global-set-key [f11] 'todo-mode)



;;;;;;;;;;;; Ros emacs ;;;;;;;;;;;;;;;
(push "~/.emacs.d/rosemacs" load-path) 

;; Load the library and start it up
;(require 'rosemacs)
;(invoke-rosemacs)

;; Optional but highly recommended: add a prefix for quick access
;; to the rosemacs commands
;(global-set-key "\C-x\C-r" ros-keymap)
(put 'downcase-region 'disabled nil)


;;; For Tex-View use Evince
(setq TeX-view-program-selection
      '(((output-dvi style-pstricks)
         "dvips and gv")
        (output-dvi "Evince")
        (output-pdf "Evince")
        (output-html "xdg-open"))
      )

(setq TeX-auto-save t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)


;;; doxymacs support
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "Qt")