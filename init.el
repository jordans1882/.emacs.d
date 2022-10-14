;; misenplace:

;; my emacs config

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate straight.el with use-package
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;; Use packages with configs


'(setq image-mode-map (make-sparse-keymap))
'(setq pdf-view-mode-map (make-sparse-keymap))
'(setq treemacs-mode-map (make-sparse-keymap))

'(progn (define-key image-mode-map (kbd ",") nil))
'(progn (define-key pdf-view-mode-map (kbd ",") nil))
'(progn (define-key pdf-view-mode-map (kbd "<normal-state> ,") nil))
'(progn (define-key image-mode-map (kbd "<normal-state> ,") nil))

(use-package emacs
  :config

  (global-auto-revert-mode t)

(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))


  (defun disable-display-line-numbers-mode-hook ()
    (display-line-numbers-mode -1))

  (add-hook 'help-mode-hook 'disable-display-line-numbers-mode-hook)


  (defun my-clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
	(comint-truncate-buffer)))


  ;; Set Warning Level
  (setq warning-minimum-level :emergency)

  ;; Appearance
  (set-face-attribute 'default nil :height 170)          ;; Font size
  (menu-bar-mode -1)                                     ;; Remove top Menu
  (tool-bar-mode -1)                                     ;; Remove top toolbar
  (scroll-bar-mode -1)                                   ;; Remove scrollbar
  (display-line-numbers-mode)
  (when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))
  (column-number-mode t)
  ;; (set-frame-parameter (selected-frame) 'alpha '(95 50)) ;; Set native alpha transparency
  ;; (add-to-list 'default-frame-alist '(alpha 95 50))      ;; Is one of these unnecessary?

  ;; Functionality
  (setq require-final-newline nil)
  (setq backup-directory-alist `(("." . "~/.emacs.d/.saves"))) ;; Set backups directory
  (setq auto-save-file-name-transforms                   ;; Set autosave directory
  `((".*" "~/.emacs.d/auto-saves/" t)))
  (setq tab-bar-select-tab-modifiers "meta")

  ;; Utility funs
  (defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
  Buffers in modified (not yet saved) state in emacs will not be reverted. They
  will be reverted though if they were modified outside emacs.
  Buffers visiting files which do not exist any more or are no longer readable
  will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
		 (not (buffer-modified-p buf)))
	(if (file-
readable-p filename) ;; If the file exists and is readable, revert the buffer.
	    (with-current-buffer buf
	      (revert-buffer :ignore-auto :noconfirm :preserve-modes))
	  ;; Otherwise, kill the buffer.
	  (let (kill-buffer-query-functions) ; No query done when killing buffer
	    (kill-buffer buf)
	    (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))


  ;; Language mode configs - Java
  (add-hook 'java-mode-hook 'hs-minor-mode)

  ;; (defun create-misenplace-cache
  ;;     "Creates folder and sets up variables for misenplace cache"
  ;;   (setq misenplace-cache-location "~/.emacs.d/misenplace-cache/")
  ;;   (if (not (f-exists? misenplace-cache-location))
  ;; 	(make-directory misenplace-cache-location))
  ;;   ;; TODO: declare and set variables and allow for custom-set
  ;;   )

  ;; hide-show config
  ;; Mode hooks
  (add-hook 'python-mode-hook 'hs-minor-mode)

  ;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . Tex-mode))
  ;;(dolist (hook '(Tex-latex-mode-hook))
    ;;(add-hook hook (lambda () (flyspell-mode 1))))

    ;; If you use tex-mode
    ;; (require 'tex-mode)`
    ;; (add-hook 'latex-mode-hook 'flyspell-mode)

    ;; If you use AUCTeX
    ;; (load "auctex.el" nil t t)`
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)


    (add-hook 'python-mode-hook 'flyspell-prog-mode)

  (defun test-pos-frame ()
    "A simple pos-frame tester fun"
    (interactive)
    (defvar my-posframe-buffer " *my-posframe-buffer*")

    (with-current-buffer (get-buffer-create my-posframe-buffer)
    (erase-buffer)
    (Lorem-ipsum-insert-paragraphs)
    (insert "Hello World"))

    (when (posframe-workable-p)
    (posframe-show my-posframe-buffer
		    :position (point))))

  (defvar dark-mode t)

  (generate-new-buffer "*std-err*")

  (defun setup-wal ()
    (progn
      (setenv "LD_THEME" "dark")
      (shell-command "export LD_THEME=dark" "*std-err*")
      (setq  dark-mode nil)))
  (setup-wal)

  (defun  toggle-dark-mode ()
    "Toggle Dark mode"
    (interactive)
    (if dark-mode
	(progn
	  (setenv "LD_THEME" "light")
	  (shell-command "export LD_THEME=light" "*std-err*")
	  (shell-command "waldark" "*std-err*")
	  (counsel-load-theme-action "doom-opera-light")
	  (setq  dark-mode nil))
	(progn
          (setenv "LD_THEME" "dark")
	  (shell-command "export LD_THEME=dark" "*std-err*")
	  (counsel-load-theme-action "base16-darktooth")
	  (setq  dark-mode t)
	  (shell-command "waldark" "*std-err*"))))

  (defun runwal ()
    (interactive)
    (shell-command "waldark" "*std-err*" "*std-err*")
    )

  ;; (run-with-timer 0 (* 1 60) 'runwal)

  (defun dawn ()
    "Set theme to dawn"
    (interactive)
    (counsel-load-theme-action "base16-gruvbox-light-soft"))
  (defun morning ()
    "Set theme to morning"
    (interactive)
    (counsel-load-theme-action "base16-gruvbox-light-hard"))
  (defun day ()
    "Set theme to day"
    (interactive)
    (counsel-load-theme-action "base16-humanoid-light"))
  (defun sunny ()
    "Set theme to sunny"
    (interactive)
    (counsel-load-theme-action "base16-mexico-light"))
  (defun dusk ()
    "Set theme to dusk"
    (interactive)
    (counsel-load-theme-action "base16-nova"))
  (defun evening ()
    "Set theme to evening"
    (interactive)
    (counsel-load-theme-action "base16-darktooth"))
  (defun late-night ()
    "Set theme to late-night"
    (interactive)
    (counsel-load-theme-action "base16-pop"))


  (defun set-target-buffer (buffer)
    "Switch to BUFFER.
 BUFFER may be a string or nil."
    (setq target-buffer buffer))

  (defun ivy-set-process-target ()
    "Switch to another buffer."
    (interactive)
    (ivy-read "Switch to buffer: " #'internal-complete-buffer
	      :keymap ivy-switch-buffer-map
	      :preselect (buffer-name (other-buffer (current-buffer)))
	      :action #'set-target-buffer
	      :matcher #'ivy--switch-buffer-matcher
	      :caller 'ivy-switch-buffer))

  (defun send-line-to-target-process ()
    "Send a line to process defined by target-buffer."
    (interactive)
    (setq proc (get-process target-buffer))
    (setq com (concat (buffer-substring (point-at-bol) (point-at-eol)) "\n"))
    (process-send-string target-buffer com)
    (next-line)
  )


  (defun increase-picom-transparency ()
    "Send a line to process defined by target-buffer."
    (interactive)
    (call-process-shell-command "inc_picom_emacs&" nil 0)
  )


  (defun decrease-picom-transparency ()
    "Send a line to process defined by target-buffer."
    (interactive)
    (call-process-shell-command "dec_picom_emacs&" nil 0)
  )


  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")


  (defun bg-elpy-shell-send-statement-and-step (&optional arg)
    (interactive "P")
    ;; Force the process to start completely by sitting a bit to avoid this warning:
    ;;
    ;;   Warning (python): Your ‘python-shell-interpreter’ doesn’t seem to support readline, yet ‘python-shell-completion-native-enable’ was t and "python" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list.  Native completions have been disabled locally.
    ;;
    ;; Refer to https://github.com/jorgenschaefer/elpy/issues/887
    ;;
    (elpy-shell-get-or-create-process 0.001)
    (elpy-shell-send-statement-and-step arg))

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Project Templates ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  ;; Python Project Template
  (defun make-python-project ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (setq python-template-repo "git@github.com:jordans1882/template-python.git")
    (setq proj-name (read-string "Enter your project name:"))
    (setq proj-dir (concatenate 'string "~/git_repos/" proj-name))
    (if (f-exists? proj-dir)
 (message (concatenate 'string proj-name " project already exists"))
 (progn (setq clone-command (concatenate 'string "git clone " python-template-repo " " proj-dir))
	(setq rm-git-command (concatenate 'string "rm -rf " proj-dir "/.git"))
	(shell-command-to-string clone-command)
	(shell-command-to-string rm-git-command)
	(magit-init proj-dir) ;; use hub for this?
	(projectile-add-known-project proj-dir))))

  (defun select-tab-first ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 1))
  (defun select-tab-second ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 2))
  (defun select-tab-third ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 3))
  (defun select-tab-fourth ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 4))
  (defun select-tab-fifth ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 5))
  (defun select-tab-sixth ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 6))
  (defun select-tab-seventh ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 7))
  (defun select-tab-eighth ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 8))
  (defun select-tab-ninth ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (tab-bar-select-tab 9))

  (defun make-r-project ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (setq r-template-repo "git@github.com:jordans1882/templater.git")
    (setq proj-name (read-string "Enter your project name:"))
    (setq proj-dir (concatenate 'string "~/git_repos/" proj-name))
    ;; TODO: Add check if project already exists for make-r-project
    (setq clone-command (concatenate 'string "git clone " r-template-repo " " proj-dir))
    (setq rm-git-command (concatenate 'string "rm -rf " proj-dir "/.git"))
    (shell-command-to-string clone-command)
    (shell-command-to-string rm-git-command)
    (magit-init proj-dir) ;; use hub for this?
    (projectile-add-known-project proj-dir)
    )

  (defun make-cpp-project ()
    "Prompt user to enter a directory name and create project."
    (interactive)
    (setq cpp-template-repo "git@github.com:jordans1882/templatepp.git")
    (setq proj-name (read-string "Enter your project name:"))
    (setq proj-dir (concatenate 'string "~/git_repos/" proj-name))
    ;; TODO: Add check if project already exists for make-cpp-project
    (setq clone-command (concatenate 'string "git clone " cpp-template-repo " " proj-dir))
    (setq rm-git-command (concatenate 'string "rm -rf " proj-dir "/.git"))
    (shell-command-to-string clone-command)
    (shell-command-to-string rm-git-command)
    (magit-init proj-dir) ;; use hub for this?
    (projectile-add-known-project proj-dir))

  (defun get-linux-os-name ()
    "Get the name of the linux operating system"
    (interactive)
    (replace-regexp-in-string
     "\n$" ""
     (shell-command-to-string
      "cat /etc/os-release | grep 'NAME' | head -n 1 | cut -c7- | sed 's/[/\"]//'")))

  (add-hook 'lisp-interaction-mode-hook 'company-mode)

  ;; Filetype modes
  (add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-interaction-mode))

  ;; Tramp config
  (setq tramp-default-method "ssh")

  ;; define function to shutdown emacs server instance
  (defun server-shutdown ()
    "Save buffers, Quit, and Shutdown (kill) server"
    (interactive)
    (save-some-buffers)
    (kill-emacs)
    )

  ;; Tab bar settings
  (set-face-attribute 'tab-bar-tab nil :inherit 'doom-modeline-panel :foreground nil :background nil)


  ;; Gant char stuff

 (defun prop_ ()
   (interactive)
   (setq properties-list '(":TYPE: Task" ":DEP: none" ":PROGRESS: 0"))

   (insert ":PROPERTIES:\n")
   (org-cycle)

   (dolist (elt properties-list)
     (insert  elt)
     (insert "\n")
     (org-cycle)
     )
   (insert ":END:\n"))
 (global-set-key (kbd "C-=") 'prop_)


  (defun get_all_headings (arr deep)
   (let ( (size (length arr)) (i 2));;get the size of the arr
     (while ( < i size );;iterate for each headings/subheadings
	(setq name (plist-get (elt (elt arr i) 1) :title))
	(setq scheduled (plist-get (elt (elt arr i) 1) :scheduled))
	(setq deadline (plist-get (elt (elt arr i) 1) :deadline))
	(setq todo (plist-get (elt (elt arr i) 1) :todo-type))
	(setq priority (plist-get (elt (elt arr i) 1) :priority))
	(setq l nil)
	(add-to-list 'l name)
	(add-to-list 'l deep)
	(if priority nil (setq priority 100))
	(add-to-list 'l priority)
	(add-to-list 'l  "na")

	(setq scheduled_vect nil)
	(if scheduled
	    (let ((tt (elt scheduled 1)) )
	      (add-to-list 'l t)
	      (setq ys (plist-get tt :year-start))
	      (setq ms (plist-get tt :month-start))
	      (setq ds (plist-get tt :day-start))
	      (setq hs (plist-get tt :hour-start))
	      (setq mins (plist-get tt :minute-start))
	      (unless hs (setq hs 0) )
	      (unless mins (setq mins 0) )
	      (setq ts (+ (* (- ys 2020) 8760) (* (calendar-day-number (list ms ds ys)) 24) hs (/ mins 60.0)))
	      (setq scheduled_vect (vector ts hs ds ms ys))
	      )
	  )

	(setq deadline_vect nil)
	(if deadline
	    (let ((tt (elt deadline 1)) )

	      (add-to-list 'l t)
	      (setq ysd (plist-get tt :year-start))
	      (setq msd (plist-get tt :month-start))
	      (setq dsd (plist-get tt :day-start))
	      (setq hsd (plist-get tt :hour-start))
	      (setq minsd (plist-get tt :minute-start))
	      (unless hsd (setq hsd 0) )
	      (unless minsd (setq minsd 0) )
	      (setq tsd (+ (* (- ysd 2020) 8760) (* (calendar-day-number (list msd dsd ysd)) 24) hsd (/ minsd 60.0)))
	      (setq deadline_vect (vector tsd hsd dsd msd ysd))
	      )
	  (if scheduled
	      (setq deadline_vect (vector (1+ ts) hs ds ms ys))
	    nil
	    )
	  )
	(if scheduled
	    (let ((a 1))
	      (add-to-list 'l (vector scheduled_vect deadline_vect))
	      (if (> minT ts)
		  (setq minT ts)
		nil)
	      (if (< maxT tsd)
		  (setq maxT tsd)
		nil)
	      )
	  (add-to-list 'l nil)
	  )

	(if (< (elt (reverse l) 2) 68)
	    (let ((a 1))
	      (add-to-list 'ordered_arr (reverse l))
	      (get_all_headings (elt arr i) (1+ deep))
	      )
	  nil
	  )
	(setq i (1+ i))
	)
     )

   )


 (defun draw-gantt ()
   "draw a gantt svg "
   (interactive)
   (get-org-heading-info)
   (require 'svg)
   (setq H 600)
   (setq W (window-pixel-width ))
   (setq sx 150)
   (setq sy 25)
   (setq Fsize 10)
   (setq svg (svg-create W H :stroke "black" ))
   (svg-rectangle svg 0 0 W H :fill "#ffffff" :stroke-width 0)

   (setq size (length ordered_arr))
   (setq i 0)
   (setq arr (reverse ordered_arr))
   (setq dt (float (- maxT minT)))
   (setq dx (float (- W sx)))
   (while (< i size)
     (let ((deep (elt (elt arr i) 1))
	    (name (elt (elt arr i) 0))
	    (visible (elt (elt arr i) 4)) )

	(if (= deep 0)
	    (svg-line svg 0 (+ sy (+ 2 (* i (+ Fsize 4)))) W (+ sy (+ 2 (* i (+ Fsize 4)))))
	  nil
	  )

	(svg-text svg name :x (+ 3 (* 8 deep)) :y (+ sy (* i (+ Fsize 4))) :stroke-width 0.5 :font-size (concat (number-to-string Fsize) "px") :font-family "Helvetica")
	(if visible
	    (let ((ts (elt (elt (elt (elt arr i) 5) 0) 0))
		  (te (elt (elt (elt (elt arr i) 5) 1) 0)))

	      (svg-rectangle svg  (+ sx (* dx (/ (- ts minT) dt))) (+ (- sy Fsize) 1 (* i (+ Fsize 4)))  (* dx (/ (- te ts) dt)) (+ -2 Fsize) :fill (color-cycle i size))
	      (if (< (elt (elt arr i) 2) 68 )
		  (let ( (day  (elt (elt (elt (elt arr i) 5) 0) 2))
			 (month  (elt (elt (elt (elt arr i) 5) 0) 3)) )
		    (svg-line svg (+ sx (* dx (/ (- ts minT) dt))) (- (- sy Fsize) 2) (+ sx (* dx (/ (- ts minT) dt))) H :stroke-dasharray 4)
		    (svg-text svg (format "%02d/%02d" day month) :x (- (+ sx (* dx (/ (- ts minT) dt))) 15) :y (+ Fsize 1) :stroke-width 0.5 :font-size (concat (number-to-string Fsize) "px") :font-family "Helvetica")
		    )
		)
	      )
	  nil
	  )
	(setq i (1+ i))
	)
     )
   (setq tnow (+ (* (- (elt (calendar-current-date) 2) 2020) 8760) (* (calendar-day-number (calendar-current-date)) 24)))
   (if (> tnow minT)
	(let ((a 1))
	  (svg-line svg (+ sx (* dx (/ (- tnow minT) dt))) (- (- sy Fsize) 2) (+ sx (* dx (/ (- tnow minT) dt))) H :stroke-dasharray 4 :stroke "red")
	  )
     nil
     )
   ;; create or get a gantt buffer
   (let (($buf (get-buffer-create "*gantt*")))
     (with-current-buffer $buf
	(erase-buffer)
	(svg-insert-image svg));;insert the svh
     (display-buffer-in-side-window $buf `((side . bottom)
					    (window-height . ,(+ 2 size)) ;;backquote + coma to evaluate var
					    (slot . 0)));;display the window at the bottom
     )
   )
 (global-set-key (kbd "C-=") 'draw-gantt)

 (defun get-org-heading-info ()
   "show org-heading-components result"
   (interactive)
   (setq z (org-element-parse-buffer 'headline))
   (setq ordered_arr nil)
   (setq minT 100000)
   (setq maxT 0)
   (get_all_headings z 0)
   )

 (defun color-cycle(i len)
   (setq fr 0.1)
   (setq center 128)
   (setq width 127)
   (setq phase (/ 6.28 3))
   (setq red (floor (+ (* (sin (+ (* fr i) 0)) width) center)))
   (setq green (floor (+ (* (sin (+ (* fr i) 2)) width) center)))
   (setq blue (floor (+ (* (sin (+ (* fr i) 4)) width) center)))
   (format "#%02x%02x%02x" red green blue)
   )



  )
(use-package ace-jump-mode
  :straight t
  )
(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )
(use-package alert
  :straight t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
(use-package alarm-clock
  :straight t
  )
(use-package all-the-icons
  :straight t
  )
(use-package all-the-icons-ivy-rich
  :straight t
  :after counsel
  :init (all-the-icons-ivy-rich-mode 1))
;; (use-package apheleia
;;   :straight t
;;   :config
;;   (apheleia-global-mode +1)
;;   )
(use-package auctex-latexmk
  :straight t
  :config
  (auctex-latexmk-setup))
(use-package auto-dim-other-buffers
  :straight (:host github :repo "mina86/auto-dim-other-buffers.el")
  :config
  (add-hook 'after-init-hook (lambda ()
			       (when (fboundp 'auto-dim-other-buffers-mode)
				 (auto-dim-other-buffers-mode t))))
  )
(use-package base16-theme
  :straight t
  ;; (load-theme 'base16-darktooth t)
  )
(use-package bash-completion
  :straight t
  :config
  (bash-completion-setup)
  )
(use-package beacon
  :straight t
  :config
  (beacon-mode 1)
  )
(use-package centered-window-mode
  :straight t
  )
(use-package cheatsheet
  :straight t
  :config
  (cheatsheet-add-group 'Common
			'(:key "C-g" :description "Escape out of current command")
			'(:key "C-x C-c" :description "leave Emacs")
			'(:key "C-n" :description "Next line")
			'(:key "C-p" :description "Previous line"))

  (cheatsheet-add-group 'MiseEnPlace-term
			'(:key "M-h" :description "Move left a buffer")
			'(:key "M-l" :description "Move right a buffer")
			'(:key "M-j" :description "Move down a buffer")
			'(:key "M-k" :description "Move down a buffer")
			'(:key "M-s" :description "Split buffer below")
			'(:key "M-v" :description "Split buffer to right")
			'(:key "M-d" :description "Delete current window")
			'(:key "," :description "Global Leader key"))

  (cheatsheet-add-group 'Vim
			'(:key ":q" :description "Quit")
			'(:key ":wq" :description "Write and quit")
			'(:key "Esc" :description "Drop back into normal mode")
			'(:key "i" :description "Drop into insert mode")
			'(:key ":" :description "Jump into menu")
			'(:key "h" :description "Move cursor left")
			'(:key "j" :description "Move cursor down")
			'(:key "k" :description "Move cursor up")
			'(:key "l" :description "Move cursor right")
			'(:key "C-u" :description "Move up half screen")
			'(:key "C-d" :description "Move down half screen")
			'(:key "w" :description "Move forward word")
			'(:key "b" :description "Move forward word")
			'(:key "ciw" :description "Change inner word")
			'(:key "ci(" :description "Change inside parens")
			'(:key "dd" :description "Delete current line")
			'(:key "c0" :description "Change from cursor to beginning of line")
			'(:key "cw" :description "Change from cursor to end of word"))
  (cheatsheet-add-group 'Tmux
			'(:key "C-a c" :description "New screen")
                        '(:key "C-a n" :description "Next screen"))

  (defun cheatsheet-group-get (grp)
    "Get cheatsheet as list of group structs, keeping defining order."
    (list (list :name grp :cheats (cheatsheet--get-group grp))))


  (defun cheatsheet--format-group-list (grp)
    "Print the whole cheatsheet."
    (let* ((cheatsheet (cheatsheet-group-get grp))
           (formatted-groups (mapcar 'cheatsheet--format-group cheatsheet))
           (formatted-cheatsheet (apply 'concat formatted-groups)))
      formatted-cheatsheet))

  (defun cheatsheet-show-group (group)
    "Create buffer and show cheatsheet."
    (interactive)
    (switch-to-buffer-other-window "*cheatsheet*")
    (setq buffer-read-only nil)
    (cheatsheet-mode)
    (erase-buffer)
    (insert (cheatsheet--format-group-list group))
    (setq buffer-read-only t)
    )

  (defun counsel-cheatsheets ()
    "Forward to `describe-function'."
    (interactive)
    (ivy-read "Cheatsheats: "
    	  (cheatsheet--cheat-groups)
    	  :action (lambda (x)
    		    (cheatsheet-show-group x))))

  )
(use-package cider
  :straight t
  )
(use-package clojure-mode
  :straight t
  )
(use-package cmake-mode
  :straight t
  :config
    ;; (defun my-set-tab-mode ()
    ;; (when (and (stringp buffer-file-name)
    ;; 		(string-match "\\CMakeLists.txt\\'" buffer-file-name))
    ;; 	(cmake-mode)))

    ;; (add-hook 'find-file-hook 'my-set-tab-mode)
  ;; TODO: add configuration to auto cmake-mode on CMakeList.txt files
  )
(use-package cmake-ide
  :straight t
  :after rtags
  :config
  (cmake-ide-setup))
(use-package command-log-mode
  :straight t
  )
(use-package company
  :straight t
  :config
  (add-hook 'shell-mode-hook 'company-mode)
  (setq company-idle-delay 0)


  ;;(add-to-list 'auto-mode-alist '("\\.h\\'" . company-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.hpp\\'" . company-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.cpp\\'" . company-mode))
  )
(use-package company-ctags
  :straight t
  :after company
  :config
    (company-ctags-auto-setup)
  )
(use-package company-irony
  :straight t
  )
(use-package company-lsp
  :straight t
  :commands company-lsp)
(use-package counsel-gtags
  :straight t
  :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)
    (with-eval-after-load 'counsel-gtags
        (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
        (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
        (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
        (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))

(use-package company-quickhelp
  :straight t
  :config
  (company-quickhelp-mode)
)

(use-package conda
  :straight t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  ;;(setq conda-env-home-directory (expand-file-name "~/anaconda3/envs"))
  )
(use-package consult
  :straight t
  ;; :config
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; ;; The :init configuration is always executed (Not lazy)
  ;; :init

  ;; ;; Optionally configure the register formatting. This improves the register
  ;; ;; preview for `consult-register', `consult-register-load',
  ;; ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-delay 0
  ;;       register-preview-function #'consult-register-format)

  ;; ;; Optionally tweak the register preview window.
  ;; ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; (advice-add #'register-preview :override #'consult-register-window)

  ;; ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; ;; Use Consult to select xref locations with preview
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)

  ;; ;; Configure other variables and modes in the :config section,
  ;; ;; after lazily loading the package.
  ;; :config

  ;; ;; Optionally configure preview. The default value
  ;; ;; is 'any, such that any key triggers the preview.
  ;; ;; (setq consult-preview-key 'any)
  ;; ;; (setq consult-preview-key (kbd "M-."))
  ;; ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; ;; For some commands and buffer sources it is useful to configure the
  ;; ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-file consult--source-project-file consult--source-bookmark
  ;;  :preview-key (kbd "M-."))

  ;; ;; Optionally configure the narrowing key.
  ;; ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; ;; Optionally make narrowing help available in the minibuffer.
  ;; ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; ;; Optionally configure a function which returns the project root directory.
  ;; ;; There are multiple reasonable alternatives to chose from.
  ;; ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;; ;;;; 2. projectile.el (projectile-project-root)
  ;; ;; (autoload 'projectile-project-root "projectile")
  ;; ;; (setq consult-project-root-function #'projectile-project-root)
  ;; ;;;; 3. vc.el (vc-root-dir)
  ;; ;; (setq consult-project-root-function #'vc-root-dir)
  ;; ;;;; 4. locate-dominating-file
  ;; ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )
;; (use-package corfu
;;   :disabled
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   ;; :bind (:map corfu-map
;;   ;;        ("TAB" . corfu-next)
;;   ;;        ("S-TAB" . corfu-previous))
;;
;;   ;; You may want to enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))
;;
;;   :config
;;
;;   ;; Alternatively enable Corfu globally.
;;   ;; This is recommended if Corfu is used with dabbrev.
;;   (corfu-global-mode)
;;
;;   ;; Optionally enable cycling for `corfu-next' and `corfu-previous'.
;;   ;; (setq corfu-cycle t)
;; )
(use-package counsel
  :straight t
  )
(use-package counsel-etags
  :straight t
  :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
    (add-hook 'prog-mode-hook
	    (lambda ()
	    (add-hook 'after-save-hook
		'counsel-etags-virtual-update-tags 'append 'local)))
  :config
    (setq counsel-etags-update-interval 60)
    (push "build" counsel-etags-ignore-directories))
(use-package counsel-tramp
  :straight t
  )
(use-package cuda-mode
  :straight t
  )
(use-package counsel-org-clock
  :straight t
  :after counsel-projectile
)
(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode)
  ;; TODO: Make check for windows - switch to native plus caching
  ;; (setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'hybrid)
  )
(use-package csharp-mode
  :straight t
  )
(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  )
(use-package dap-java
  :ensure nil
  :config
    (dap-register-debug-template "Runner"
				(list :type "java"
				    :request "launch"
				    :args ""
				    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
				    :projectName "myapp"
				    :mainClass "edu.msu.utilities.LinSpace"
				    :env '(("DEV" . "1"))))
)
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '(;; (recents  . 5)
			  (projects . 5)
			  ;; (bookmarks . 5)
			  (agenda . 5)
			  ;;(registers . 5)
			  ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/_assets/mise_en_place.png")
  ;; (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-projects-switch-function 'projectile-persp-switch-project)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Match just one TODO keyword.
  ;; (setq dashboard-match-agenda-entry "NEXT=\"NEXT\"")
  (setq dashboard-match-agenda-entry "TODO=\"NEXT\"")
  ;; ;; Match multiple keywords.
  ;; (setq dashboard-match-agenda-entry
  ;; "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"")
  ;;
  ;; ;; Match everything except WAITING.
  ;; (setq dashboard-match-agenda-entry
  ;; "-TODO=\"WAITING\"")
  (setq dashboard-projects-show-base t)


  )
(use-package deft
  :straight t
  :config
  (require 'deft)
  (setq deft-directory "~/org")
  )
;; (use-package dimmer
;;   :straight t
;;   :disabled
;;   :config
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-helm)
;;   (dimmer-mode t)
;;   (setq dimmer-adjustment-mode ":background")
;;   (setq dimmer-fraction 0.6)
;;   )
(use-package dockerfile-mode
  :straight t
  )
;; (use-package dogears
;;   :straight t
;;   :config
;;   (add-to-list 'dogears-hooks 'consult-after-jump-hook)
;;
;;   )
(use-package doom-modeline
      :straight t
      :hook (after-init . doom-modeline-mode)
      :config
	; How tall the mode-line should be. It's only respected in GUI.
	;; If the actual char height is larger, it respects the actual height.
	(setq doom-modeline-height 25)

	;; How wide the mode-line bar should be. It's only respected in GUI.
	(setq doom-modeline-bar-width 4)

	;; Whether to use hud instead of default bar. It's only respected in GUI.
	(setq doom-modeline-hud nil)

	;; The limit of the window width.
	;; If `window-width' is smaller than the limit, some information won't be displayed.
	(setq doom-modeline-window-width-limit fill-column)

	;; How to detect the project root.
	;; nil means to use `default-directory'.
	;; The project management packages have some issues on detecting project root.
	;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
	;; to hanle sub-projects.
	;; You can specify one if you encounter the issue.
	(setq doom-modeline-project-detection 'auto)

	;; Determines the style used by `doom-modeline-buffer-file-name'.
	;;
	;; Given ~/Projects/FOSS/emacs/lisp/comint.el
	;;   auto => emacs/lisp/comint.el (in a project) or comint.el
	;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
	;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
	;;   truncate-with-project => emacs/l/comint.el
	;;   truncate-except-project => ~/P/F/emacs/l/comint.el
	;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
	;;   truncate-all => ~/P/F/e/l/comint.el
	;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
	;;   relative-from-project => emacs/lisp/comint.el
	;;   relative-to-project => lisp/comint.el
	;;   file-name => comint.el
	;;   buffer-name => comint.el<2> (uniquify buffer name)
	;;
	;; If you are experiencing the laggy issue, especially while editing remote files
	;; with tramp, please try `file-name' style.
	;; Please refer to https://github.com/bbatsov/projectile/issues/657.
	(setq doom-modeline-buffer-file-name-style 'auto)

	;; Whether display icons in the mode-line.
	;; While using the server mode in GUI, should set the value explicitly.
	(setq doom-modeline-icon (display-graphic-p))

	;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
	(setq doom-modeline-major-mode-icon t)

	;; Whether display the colorful icon for `major-mode'.
	;; It respects `all-the-icons-color-icons'.
	(setq doom-modeline-major-mode-color-icon t)

	;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
	(setq doom-modeline-buffer-state-icon t)

	;; Whether display the modification icon for the buffer.
	;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
	(setq doom-modeline-buffer-modification-icon t)

	;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
	(setq doom-modeline-unicode-fallback nil)

	;; Whether display the minor modes in the mode-line.
	(setq doom-modeline-minor-modes nil)

	;; If non-nil, a word count will be added to the selection-info modeline segment.
	(setq doom-modeline-enable-word-count nil)

	;; Major modes in which to display word count continuously.
	;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
	;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
	;; remove the modes from `doom-modeline-continuous-word-count-modes'.
	(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

	;; Whether display the buffer encoding.
	(setq doom-modeline-buffer-encoding t)

	;; Whether display the indentation information.
	(setq doom-modeline-indent-info nil)

	;; If non-nil, only display one number for checker information if applicable.
	(setq doom-modeline-checker-simple-format t)

	;; The maximum number displayed for notifications.
	(setq doom-modeline-number-limit 99)

	;; The maximum displayed length of the branch name of version control.
	(setq doom-modeline-vcs-max-length 12)

	;; Whether display the workspace name. Non-nil to display in the mode-line.
	(setq doom-modeline-workspace-name t)

	;; Whether display the perspective name. Non-nil to display in the mode-line.
	(setq doom-modeline-persp-name t)

	;; If non nil the default perspective name is displayed in the mode-line.
	(setq doom-modeline-display-default-persp-name nil)

	;; If non nil the perspective name is displayed alongside a folder icon.
	(setq doom-modeline-persp-icon t)

	;; Whether display the `lsp' state. Non-nil to display in the mode-line.
	(setq doom-modeline-lsp t)

	;; Whether display the GitHub notifications. It requires `ghub' package.
	(setq doom-modeline-github t) ;; default nil

	;; The interval of checking GitHub.
	(setq doom-modeline-github-interval (* 30 60))

	;; Whether display the modal state icon.
	;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
	(setq doom-modeline-modal-icon t)

	;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
	(setq doom-modeline-mu4e nil)
	;; also enable the start of mu4e-alert
	(mu4e-alert-enable-mode-line-display)

	;; Whether display the gnus notifications.
	(setq doom-modeline-gnus t)

	;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
	(setq doom-modeline-gnus-timer 2)

	;; Wheter groups should be excludede when gnus automatically being updated.
	(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

	;; Whether display the IRC notifications. It requires `circe' or `erc' package.
	(setq doom-modeline-irc t)

	;; Function to stylize the irc buffer names.
	(setq doom-modeline-irc-stylize 'identity)

	;; Whether display the environment version.
	(setq doom-modeline-env-version t)
	;; Or for individual languages
	(setq doom-modeline-env-enable-python t)
	(setq doom-modeline-env-enable-ruby t)
	(setq doom-modeline-env-enable-perl t)
	(setq doom-modeline-env-enable-go t)
	(setq doom-modeline-env-enable-elixir t)
	(setq doom-modeline-env-enable-rust t)

	;; Change the executables to use for the language version string
	(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
	(setq doom-modeline-env-ruby-executable "ruby")
	(setq doom-modeline-env-perl-executable "perl")
	(setq doom-modeline-env-go-executable "go")
	(setq doom-modeline-env-elixir-executable "iex")
	(setq doom-modeline-env-rust-executable "rustc")

	;; What to display as the version while a new one is being loaded
	(setq doom-modeline-env-load-string "...")

	;; Hooks that run before/after the modeline version string is updated
	(setq doom-modeline-before-update-env-hook nil)
	(setq doom-modeline-after-update-env-hook nil)
	(add-hook 'inferior-ess-mode-hook
	    (lambda ()
		(add-to-list 'mode-line-process '(:eval (nth ess--busy-count ess-busy-strings)))))
      )
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  )
(use-package dumb-jump
  :straight t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )
;; (use-package eaf
;;   :straight t
;;   :config
;;   :load-path "/usr/share/emacs/site-lisp/eaf"
;;   ; :load-path "~/.emacs.d/straight/repos/eaf" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   )
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))
(use-package epc
  :straight t
  )
(use-package eglot
  :straight t
  )
(use-package egg-timer
  :straight t
  )
(use-package elgantt
  :straight (elgantt :type git
                     :host github
                     :repo "legalnonsense/elgantt"
                     :branch "master")
  :config
  (setq elgantt-agenda-files "~/git_repos/test/todo.org")
  (setq elgantt-user-set-color-priority-counter 0)
  (elgantt-create-display-rule draw-scheduled-to-deadline
    :parser ((elgantt-color . ((when-let ((colors (org-entry-get (point) "ELGANTT-COLOR")))
                                 (s-split " " colors)))))
    :args (elgantt-scheduled elgantt-color elgantt-org-id)
    :body ((when elgantt-scheduled
             (let ((point1 (point))
                   (point2 (save-excursion
                             (elgantt--goto-date elgantt-scheduled)
                             (point)))
                   (color1 (or (car elgantt-color)
                               "black"))
                   (color2 (or (cadr elgantt-color)
                               "red")))
               (when (/= point1 point2)
                 (elgantt--draw-gradient
                  color1
                  color2
                  (if (< point1 point2) point1 point2) ;; Since cells are not necessarily linked in
                  (if (< point1 point2) point2 point1) ;; chronological order, make sure they are sorted
                  nil
                  `(priority ,(setq elgantt-user-set-color-priority-counter
                                    (1- elgantt-user-set-color-priority-counter))
                             ;; Decrease the priority so that earlier entries take
                             ;; precedence over later ones (note: it doesn’t matter if the number is negative)
                             :elgantt-user-overlay ,elgantt-org-id)))))))
    (elgantt-create-display-rule show-hashtag-links
    :args (elgantt-hashtag)
    :post-command-hook t ;; update each time the point is moved
    :body ((elgantt--clear-juxtapositions nil nil 'hashtag-link) ;; Need to clear the last display
	    (when elgantt-hashtag ;; only do it if there is a hashtag property at the cell
	    (elgantt--connect-cells :elgantt-alltags elgantt-hashtag 'hashtag-link '(:foreground "red")))))

    (elgantt-create-action follow-hashtag-link-forward
    :args (elgantt-alltags)
    :binding "C-M-f"
    :body ((when-let* ((hashtag (--first (s-starts-with-p "#" it)
					elgantt-alltags))
			(point (car (elgantt--next-match :elgantt-alltags hashtag))))
	    (goto-char point))))

    (elgantt-create-action follow-hashtag-link-backward
    :args (elgantt-alltags)
    :binding "C-M-b"
    :body ((when-let* ((hashtag (--first (s-starts-with-p "#" it)
					elgantt-alltags))
			(point (car (elgantt--previous-match :elgantt-alltags hashtag))))
	    (goto-char point))))
    )
(use-package elpy
  :straight t
  :init
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  )
(use-package ein
  :straight t
  )
;; (use-package webkit-color-picker
;;   :straight t
;;   )
(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode))
(use-package ess
  :straight t
  :config

  (add-hook 'ess-r-help-mode-hook 'disable-display-line-numbers-mode-hook)
  (add-hook 'inferior-ess-mode-hook 'disable-display-line-numbers-mode-hook)
  (setq ess-use-flymake nil) ;; disable Flymake
  (add-to-list 'auto-mode-alist '("\\.r\\'" . ess-r-mode))
  (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

  (add-hook 'ess-mode-hook '(lambda () (define-key ess-mode-map (kbd "M-<RET>") 'ess-eval-region-or-line-visibly-and-step)))
  (add-hook 'ess-mode-hook '(lambda () (define-key ess-mode-map (kbd "C-S-<RET>") 'ess-eval-region-or-function-or-paragraph-and-step)))

  (defun ess-eval-structure (&optional vis)
    "Send the current line to the inferior ESS process. VIS has same meaning as for `ess-eval-region'."
    (interactive "P")
    (let* ((beg (point-at-bol))
	   (end (point-at-eol))
	   (cmd (buffer-substring beg end))
	   (msg (format "Structure for: %s" cmd)))
      (ess-send-string (ess-get-process) (concat (concat "str(" cmd) ")" ) t))
  )

  (defun ess-eval-structure (&optional vis)
    "Send the current line to the inferior ESS process. VIS has same meaning as for `ess-eval-region'."
    (interactive "P")
    (let* ((beg (point-at-bol))
	   (end (point-at-eol))
	   (cmd (thing-at-point 'word 'no-properties))
	   (msg (format "Structure for: %s" cmd)))
      (ess-send-string (ess-get-process) (concat (concat "str(" cmd) ")" ) t))
  )

 (defun asb-read-into-string (buffer)
 (with-current-buffer buffer
   (buffer-string)))

 (defun asb-ess-R-object-popup (r-func)
   "R-FUNC: The R function to use on the object.
 Run R-FUN for object at point, and display results in a popup."
   (let ((objname (current-word))
         (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
     (if objname
         (progn
           (ess-command (concat "class(" objname ")\n") tmpbuf)
           (let ((bs (asb-read-into-string tmpbuf)))
             (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                 (progn
                   (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                   (let ((bs (asb-read-into-string tmpbuf)))
                     (popup-tip bs
 		       :scroll-bar t)))))))
   (kill-buffer tmpbuf)))

 (defun asb-ess-R-object-popup-str ()
   "Popup structure of R object."
   (interactive)
   (asb-ess-R-object-popup "str"))

 (defun asb-ess-R-object-popup-cls ()
   "Popup class of R object."
   (interactive)
   (asb-ess-R-object-popup "class"))

 (defun asb-ess-R-object-popup-interactive (r-func)
   "R Object Interactive Popup.
 R-FUNC: An R function to use on object"
   (interactive "sR function to execute: ")
   (asb-ess-R-object-popup r-func))
)
(use-package evil
   :straight t
   :init
   (setq evil-want-keybinding nil)
   :config
   (evil-mode 1)
   (setq-default evil-escape-delay 0.05)
   (add-to-list 'evil-normal-state-modes 'ess-r-help-mode)
   (add-to-list 'evil-normal-state-modes 'inferior-ess--mode)
   (add-to-list 'evil-normal-state-modes 'package-menu-mode)
   (add-to-list 'evil-normal-state-modes 'magit-mode)
   (add-to-list 'evil-emacs-state-modes 'Compilation)
   (add-to-list 'evil-emacs-state-modes 'Summary)
   (add-to-list 'evil-emacs-state-modes 'Article)
   (add-to-list 'evil-emacs-state-modes 'DocView)

   ;; Create user keymap (personal leader)
   (defvar my-leader-map (make-sparse-keymap)
     "Keymap for \"leader key\" shortcuts.")
   (defvar my-second-leader-map (make-sparse-keymap)
     "Keymap for \"leader key\" shortcuts.")

   (define-key my-leader-map "wd" 'evil-window-delete)

   ;; Define/rebind evil-mode-normal-state maps

   (define-key evil-normal-state-map (kbd "K") 'evil-goto-definition)
   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
   (define-key evil-normal-state-map (kbd "q") 'evil-delete-buffer)
   (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
   (define-key evil-normal-state-map (kbd "H-l") 'evil-window-increase-width)
   (define-key evil-normal-state-map (kbd "H-h") 'evil-window-decrease-width)
   (define-key evil-normal-state-map (kbd "H-j") 'evil-window-increase-height)
   (define-key evil-normal-state-map (kbd "H-k") 'evil-window-decrease-height)
   (define-key evil-normal-state-map "," my-leader-map)
   (define-key evil-normal-state-map " " my-second-leader-map)
   (define-key evil-visual-state-map "," my-leader-map)
   (define-key evil-visual-state-map " " my-second-leader-map)
   (define-key evil-normal-state-map (kbd "/") 'swiper-isearch)
   (define-key evil-normal-state-map (kbd "?") 'swiper-isearch-backward)

   ;; Manually add in my-leader-map bindings to states
   ;; (define-key compilation-mode-map "," my-leader-map)
   ;; (define-key compilation-mode-map " " my-second-leader-map)
   )
(use-package evil-collection
  :straight t
  :config
  (evil-collection-init)
  )
(use-package evil-snipe
  :straight t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))
(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))
;; (use-package evil-nerd-commenter
;;   :straight t
;;   )
(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package fill-column-indicator
  :straight t
  )
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode t)

  ;; Enable for other modes ;; TODO: Wait... but isn't it already global? What do I not want it for?
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (setq flycheck-lintr-linters "with_defaults(default = default_linters, line_length_linter = line_length_linter(120), assignment_linter = NULL)")
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

  (defun my-save-word ()
  (interactive)
  (let ((current-location (point))
	(word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


)
(use-package flycheck-grammarly
  :straight t
  :config
    (with-eval-after-load 'flycheck
	(flycheck-grammarly-setup))
    )
(use-package flycheck-rust
  :straight t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)
(use-package flycheck-google-cpplint
  :straight t
  :after flycheck
  :config
  (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))
 (custom-set-variables
  '(flycheck-googlelint-verbose "3")
  '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
  ;; '(flycheck-googlelint-root "project/src")
  '(flycheck-googlelint-linelength "120"))
)
(use-package flycheck-clojure
  :straight t
  )
(use-package forge
  :straight t
  )
(use-package format-all
  :straight t
  )
(use-package fzf
  :straight t
  )
(use-package geiser-mit
  :straight t)
(use-package general
  :straight t
  :after (:all which-key hydra org-super-agenda)
  :config
  ;; (general-evil-setup t) ;; TODO: investigate commented code

  (general-create-definer misenplace/leader-keys ;; Define leader key
			  :states '(normal insert visual emacs override)
			  :prefix ","
			  :global-prefix "C-,"
			  )


  (general-define-key ;; global, evil-normal-state- states: normal, visual, insert
     :states '(normal)
     :keymaps '(global-map evil-normal-state-map)
     ;; "f" 'link-hint-open-link ;; TODO: move to org-mode override??
     "C-o" 'gumshoe-persp-backtrack-back
     "C-i" 'gumshoe-persp-backtrack-forward
     )

  (general-define-key ;; global, evil-normal-state - states: normal, visual, insert
     :states '(normal visual insert)
     :keymaps '(global-map evil-normal-state-map override)
     ;; "C-c C-c" 'evilnc-comment-or-uncomment-lines ;; TODO: find a new bind for commenting

     "H-C-=" 'increase-picom-transparency
     "H-C--" 'decrease-picom-transparency
     "C-=" 'text-scale-increase
     "C--" 'text-scale-decrease
     "C-w" 'tab-bar-close-tab
     "M-1" 'select-tab-first
     "M-2" 'select-tab-second
     "M-3" 'select-tab-third
     "M-4" 'select-tab-fourth
     "M-5" 'select-tab-fifth
     "M-6" 'select-tab-sixth
     "M-7" 'select-tab-seventh
     "M-8" 'select-tab-eighth
     "M-9" 'select-tab-ninth
     "M-C-j" 'evil-window-decrease-height
     "M-C-k" 'evil-window-increase-height
     "M-C-h" 'evil-window-decrease-width
     "M-C-l" 'evil-window-increase-width
     "M-l" 'evil-window-right
     "M-j" 'evil-window-down
     "M-k" 'evil-window-up
     "M-h" 'evil-window-left
     "M-d" 'evil-window-delete
     "M-r" 'tab-bar-rename-tab
     "M-s" 'evil-window-split
     "M-v" 'evil-window-vsplit
     "M-S-j" 'evil-rotate-upwards
     "M-S-k" 'evil-rotate-downwards
     "M-S-<tab>" 'tab-bar-switch-to-prev-tab
     "M-<iso-lefttab>" 'tab-bar-switch-to-prev-tab
     "M-<tab>" 'tab-bar-switch-to-next-tab
     "M-C-r" 'restart-emacs
     "M-S-c" 'evil-window-delete
     "M-q" 'evil-window-delete
     "C-q" 'evil-delete-buffer
     "C-Q" 'evil-quit
     "C-o" 'gumshoe-persp-backtrack-back
     "C-t" 'tab-bar-new-tab
     "C-i" 'gumshoe-persp-backtrack-forward
     "C-<SPACE>" 'send-line-to-target-process
     "C-<return>" 'send-line-to-target-process
     "C-<tab>" 'tab-bar-switch-to-next-tab
     [(control shift iso-lefttab)] 'tab-bar-switch-to-prev-tab
     "ESC ESC ESC" 'evil-normal-state
     ;; "C-k" 'kill-word
     "C-k" 'projectile-compile-project
     )

  ;; TODO: add override to keymaps?
  (general-define-key ;; evil insert-mode bindings
     :states '(insert)
     :keymaps '(global-map evil-normal-state-map)
     "<tab>" 'self-insert-command
     )

  ;; (global-set-key [(control shift iso-lefttab)] 'tab-previous))

  (general-define-key ;; Evil normal and visual mode bindings
     :states '(normal visual)
     :keymaps '(global-map evil-normal-state-map override)
     "q" 'keyboard-escape-quit
     "zj" 'origami-next-fold
     "zM" 'hs-hide-level
     "zk" 'origami-previous-fold
     "zn" 'origami-next-fold
     "zp" 'origami-previous-fold
     "C-o" 'gumshoe-persp-backtrack-back
     "C-i" 'gumshoe-persp-backtrack-forward
     )









  (general-define-key ;; treemacs
     :states '(normal visual treemacs)
     :keymaps '(treemacs-mode-map)
     "C-o" 'gumshoe-persp-backtrack-back
     "C-i" 'gumshoe-persp-backtrack-forward
     "C-<tab>" 'tab-bar-switch-to-next-tab
     "M-<tab>" 'tab-bar-switch-to-next-tab
     "M-l" 'evil-window-right
     "r" 'treemacs-rename-file
     "l" 'treemacs-toggle-node
     "h" 'treemacs-collapse-parent-node
     "D" 'treemacs-delete-file
     "C-r" 'treemacs-refresh
  )

  (general-define-key ;; company
     :states '(normal visual company)
     :keymaps '(company-active-map company-search-map)
     "C-n" 'company-select-next
     "C-p" 'company-select-previous
     "C-t" 'company-search-toggle-filtering
     )


  (general-define-key ;; lisp interaction
     :states '(normal visual)
     :keymaps '(lisp-interaction-mode-map)
     "M-<RET>" 'eval-defun
     "C-o" 'gumshoe-persp-backtrack-back
     "C-i" 'gumshoe-persp-backtrack-forward
  )

  (general-define-key ;; Treemacs modemap bindings
     :states '(normal visual)
     :keymaps '(treemacs-mode-map)
     "M-l" 'evil-window-right
     "M-d" 'treemacs-quit
  )

  (general-define-key ;; Org-agenda modemap bindings
     :states '(normal visual)
     :keymaps '(org-agenda-mode-map)
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line
  )

  (general-define-key ;; org-super-agenda-header-map
     :states '(normal visual)
     :keymaps '(org-super-agenda-header-map)

     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line
  )

  (general-define-key ;; python-mode-map
     :states '(normal visual)
     :keymaps '(sh-mode-map)
     "M-<RET>" '(bg-elpy-shell-send-statement-and-step :which-key "send")
     ;; ",l" '(:ignore t :which-key "Python")
     ;; ",la" '(elpy-goto-assignment :which-key "goto-Assignment")
     )


  (general-define-key ;; python-mode-map
     :states '(normal visual)
     :keymaps '(pdf-view-mode-map)
     ;; "M-<RET>" '(bg-elpy-shell-send-statement-and-step :which-key "send")
     ",w" '(:ignore t :which-key "Window")
     ",wh" '(:ignore t :which-key "left")
     ",wl" '(:ignore t :which-key "right")
     ",wj" '(:ignore t :which-key "down")
     ",wk" '(:ignore t :which-key "up")
     )

  (general-define-key ;; python-mode-map
     :states '(normal visual)
     :keymaps '(image-mode-map)
     ;; "M-<RET>" '(bg-elpy-shell-send-statement-and-step :which-key "send")
     ",w" '(:ignore t :which-key "Window")
     ",wh" '(:ignore t :which-key "left")
     ",wl" '(:ignore t :which-key "right")
     ",wj" '(:ignore t :which-key "down")
     ",wk" '(:ignore t :which-key "up")
     )


  (general-define-key ;; python-mode-map
     :states '(normal visual)
     :keymaps '(python-mode-map)
     ;; "M-<RET>" '(bg-elpy-shell-send-statement-and-step :which-key "send")
     ",l" '(:ignore t :which-key "Python")
     ",la" '(elpy-goto-assignment :which-key "goto-Assignment")
     ",lc" '(elpy-shell-send-defclass-and-step :which-key "send defClass")
     ",ld" '(elpy-goto-definition :which-key "goto-Definition")
     ",le" '(conda-environment-activate :which-key "environment (conda)")
     ",lf" '(elpy-format-code :which-key "Format")
     ;; ",ldI" '(asb-ess-R-object-popup-interactive :which-key "interactive inspect")
     ;; ",ldc" '(asb-ess-R-object-popup-cls :which-key "class")
     ",lh" '(elpy-doc :which-key "Help")
     ",lk" '(elpy-shell-kill :which-key "Kill")
     ;; ",li" '(asb-ess-R-object-popup-str :which-key "inspect")
     ;; ",lI" '(ess-r-devtools-install-package :which-key "install package")
     ;; ",lL" '(ess-r-devtools-install-package :which-key "load package")
     ;; ",lo" '(ess-rdired :which-key "object")
     ;; ",lp" '(:ignore t :which-key "project")
     ;; ",lpb" '(ess-r-devtools-build :which-key "build")
     ;; ",lpc" '(ess-r-devtools-check-package :which-key "check")
     ;; ",lpt" '(ess-r-devtools-test-package :which-key "test")
     ;; ",lq" '(ess-watch-quit :which-key "quit")
     ;; ",lt" '(ess-eval-structure :which-key "structure")
     )


  (general-define-key ;; TeX mode
     :states '(normal visual)
     :keymaps '(TeX-mode-map)
     ",hh" '(TeX-documentation-texdoc :which-key "documentation")
     )


  (general-define-key ;; Racket mode
     :states '(normal visual)
     :keymaps '(racket-mode-map)
     "M-<RET>" 'racket-send-definition
     ",hh" '(racket-repl-describe :which-key "documentation")
     )

  (general-define-key ;; ESS R mode
     :states '(normal visual)
     :keymaps '(ess-r-mode-map)

     "M-<RET>" 'ess-eval-region-or-line-and-step
     "<C-M-return>" 'ess-eval-function-or-paragraph-and-step

     ;; "M-<RET>" 'r-vterm-send-region-or-current-line
     ;; "<C-M-return>" 'r-vterm-send-region-or-current-line


     ",l" '(:ignore t :which-key "R")
     ",ldi" '(asb-ess-R-object-popup-str :which-key "inspect")
     ",ldI" '(asb-ess-R-object-popup-interactive :which-key "interactive inspect")
     ",ldc" '(asb-ess-R-object-popup-cls :which-key "class")
     ",lh" '(ess-display-help-on-object :which-key "help")
     ",li" '(asa-ess-R-object-popup-str :which-key "introspect")
     ",lI" '(ess-r-devtools-install-package :which-key "install package")
     ",lL" '(ess-r-devtools-install-package :which-key "load package")
     ",lo" '(ess-rdired :which-key "object")
     ",lp" '(:ignore t :which-key "project")
     ",lpb" '(ess-r-devtools-build :which-key "build")
     ",lpc" '(ess-r-devtools-check-package :which-key "check")
     ",lpt" '(ess-r-devtools-test-package :which-key "test")
     ",lq" '(ess-watch-quit :which-key "quit")
     ",lt" '(ess-eval-structure :which-key "structure")
     )

  (general-define-key ;; org-mode bindings
     :states '(normal visual)
     :keymaps '(org-mode-map)
     "M-C-<return>" 'org-babel-execute-src-block
     "M-C-e" 'org-latex-export-to-pdf
     "M-o" 'org-open-at-point
     "M-l" 'evil-window-right
     "M-j" 'evil-window-down
     "M-k" 'evil-window-up
     "M-h" 'evil-window-left
     "M-SPC" 'send-line-to-target-process
     )

  (general-define-key ;; python
     :states '(normal visual)
     :keymaps '(python-mode-map)

     ;; General
     "M-<RET>" 'elpy-shell-send-statement-and-step
     )

  (general-define-key ;; python
     :states '(normal visual)
     :keymaps '(python-mode-map)
     :prefix ","

     "c" '(:ignore t :which-key "Python")
     "ca" '(elpy-goto-assignment :which-key "goto-Assignment")
     "c <RET>" '(elpy-shell-send-defclass-and-step :which-key "send defClass")
     "cg" '(:ignore t :which-key "GoTo")
     "cgg" '(elpy-goto-definition :which-key "goto-Definition")
     "ce" '(conda-environment-activate :which-key "environment (conda)")
     "cf" '(elpy-format-code :which-key "Format")
     "ch" '(elpy-doc :which-key "Help")
     "ck" '(elpy-shell-kill :which-key "Kill")

     ;; Tests
     "x" '(:ignore t :which-key "tests")
     "hh" '(elpy-doc :which-key "docs")
     "xx" '(python-pytest-function-dwim :which-key "this")
     "xm" '(python-pytest-dispatch :which-key "menu")
     "xf" '(python-pytest-file :which-key "file"))


  (misenplace/leader-keys ;; Leader bindings
   ;; Agenda
   "a" '(:ignore t :which-key "agenda")
   "aa" '(org-agenda :which-key "agenda")
   "as" '(org-schedule :which-key "schedule")
   "an" '(org-projectile-capture-for-current-project :which-key "capture project")
   "ac" '(:ignore t :which-key "clock")
   "acc" '(counsel-org-clock-goto :which-key "clock goto")
   "aci" '(org-clock-in :which-key "clock in")
   "aco" '(org-clock-out :which-key "clock out")
   "ach" '(counsel-org-clock-history :which-key "clock history")
   "ag" '(:ignore t :which-key "goto")
   "agp" '(org-projectile-goto-location-for-project :which-key "project org")
   "at" '(org-todo :which-key "todo")
   "ap" '(org-pomodoro :which-key "pomodoro")

   ;; Buffers
   "b" '(:ignore t :which-key "buffers")
   "bb" '(persp-counsel-switch-buffer :which-key "find")
   "bB" '(switch-to-buffer :which-key "Find")
   "bc" '(evil-buffer-new :which-key "create")
   "bl" '(next-buffer :which-key "next")
   "bp" '(previous-buffer :which-key "previous")
   "bn" '(evil-next-buffer :which-key "next")
   "bp" '(evil-previous-buffer :which-key "previous")
   "bd" '(evil-delete-buffer :which-key "delete")
   "br" '(rename-buffer :which-key "rename")
   "bR" '(modi/revert-all-file-buffers :which-key "revert all")

   ;; Dumb-jump
   "c" '(:ignore t :which-key "code")
   "cc" '(counsel-imenu :which-key "go")

   ;; Dumb-jump
   "d" '(:ignore t :which-key "dumb-jump")
   "dd" '(evil-goto-definition :which-key "go")
   "db" '(dumb-jump-back :which-key "back")
   "do" '(dumb-jump-go-other-window :which-key "go other")
   "dq" '(dumb-jump-quick-look :which-key "quick-look")

   ;; Errors
   "e" '(:ignore t :which-key "errors")
   "ee" '(flycheck-list-errors :which-key "goto")
   "e/" '(counsel-flycheck :which-key "counsel")
   "ej" '(flycheck-next-error :which-key "next")
   "en" '(flycheck-next-error :which-key "next")
   "ek" '(flycheck-previous-error :which-key "previous")
   "ep" '(flycheck-previous-error :which-key "previous")

   ;; Files
   "f" '(:ignore t :which-key "files")
   "ff" '(treemacs :which-key "menu")
   "fc" '(treemacs-create-file :which-key "create file")
   "fC" '(treemacs-create-dir :which-key "create dir")
   "fd" '(treemacs-delete :which-key "delete")
   "fe" '(:ignore t :which-key "edit")
   "fea" '(edit-awesomerc :which-key "awesomerc")
   "feb" '(edit-bashrc :which-key "bashrc")
   "fee" '(edit-config :which-key "emacsrc")
   "feq" '(edit-qutebrowser :which-key "qutebrowserrc")
   "fer" '(reload-config :which-key "reload emacs")
   "fev" '(edit-vimrc :which-key "vimrc")
   "fey" '(edit-yas-config :which-key "yasrc")
   "fq" '(treemacs-quit :which-key "quit")
   "fs" '(treemacs-visit-node-horizontal-split :which-key "split")
   "fv" '(treemacs-visit-node-vertical-split :which-key "vsplit")

   ;; Git
   "g" '(:ignore t :which-key "git")
   "gb" '(magit-branch :which-key "branch-switch")
   "gB" '(magit-branch-create :which-key "branch-create")
   "gc" '(magit-commit :which-key "commit")
   "gj" '(git-gutter:next-diff :which-key "next-diff")
   "gg" '(magit-status :which-key "status")
   "gk" '(git-gutter:previous-diff :which-key "previous-diff")
   "gl" '(magit-log :which-key "log")
   "gn" '(git-gutter:next-diff :which-key "next-diff")
   "gp" '(git-gutter:previous-diff :which-key "previous-diff")
   "gs" '(git-gutter:stage-hunk :which-key "stage hunk")
   "gS" '(magit-stage :which-key "stage")
   "gU" '(magit-unstage :which-key "unstage")
   "g<" '(magit-pull :which-key "pull")
   "g>" '(magit-push :which-key "push")

   ;; Help
   "h" '(:ignore t :which-key "help")
   "hc" '(counsel-cheatsheets :which-key "cheatsheets")
   "hm" '(describe-mode :which-key "mode")
   "hk" '(describe-key :which-key "key")
   "hf" '(counsel-describe-function :which-key "function")
   "hv" '(counsel-describe-variable :which-key "variable")
   "hs" '(counsel-describe-symbol :which-key "symbol")

   ;; Imenu
   "i" '(:ignore t :which-key "imenu")
   "ii" '(imenu-list :which-key "list")

   ;; Jump
   "j" '(:ignore t :which-key "jump")
   "jb" '(counsel-cheatsheets :which-key "back")
   "jd" '(dumb-jump-go :which-key "definition")
   "jt" '(projectile-find-tag :which-key "tag")
   "jj" '(evil-ace-jump-char-mode :which-key "jump")
   "jw" '(evil-ace-jump-word-mode :which-key "word")
   "jl" '(evil-ace-jump-line-mode :which-key "line")


   "ml" '(hide-mode-line-mode :which-key "toggle mode line")

   ;; Org
   "o" '(:ignore t :which-key "org")
   "oa" '(:ignore t :which-key "agenda")
   "oas" '(org-schedule :which-key "schedule")
   "oad" '(org-deadline :which-key "deadline")
   "oap" '(org-set-property :which-key "property")
   "od" '(deft :which-key "deft")
   "oc" '(:ignore t :which-key "capture/clock")
   "occ" '(org-capture :which-key "capture")
   "oci" '(org-clock-in :which-key "clock-in")
   "oco" '(org-clock-out :which-key "clock-out")
   "or" '(:ignore t :which-key "ref")
   "orr" '(org-ref-insert-link :which-key "search")

   ;; Projects
   "p" '(:ignore t :which-key "projects")
   "pA" '(projectile-add-known-project :which-key "add")
   "pa" '(counsel-projectile-org-agenda :which-key "agenda")
   "pc" '(counsel-projectile-org-capture :which-key "capture")
   "pd" '(counsel-projectile-find-dir :which-key "directory")
   "pm" '(projectile-compile-project :which-key "compile")
   "po" '(projectile-persp-switch-project :which-key "open")
   "pp" '(persp-switch :which-key "switch")
   "pf" '(counsel-projectile-find-file :which-key "file")
   ;; "pq" '(projectile-kill-buffers :which-key "quit")
   "pq" '(persp-kill :which-key "quit")
   "pr" '(counsel-projectile-rg :which-key "ripgrep")
   "ps" '(projectile-run-vterm :which-key "shell")
   "pt" '(:ignore t :which-key "test")
   "ptt" '(projectile-test-project :which-key "test all")
   "ptt" '(projectile-find-test-file :which-key "file")
   "pT" '(:ignore t :which-key "tags")
   "pTT" '(projectile-find-tag :which-key "find")
   "pTr" '(projectile-tag-regenerate :which-key "regenerate")
   ;; (define-key my-leader-map "p[" 'projectile-previous-project-buffer)
   ;; (define-key my-leader-map "p]" 'projectile-next-project-buffer)


   "q" '(:ignore t :which-key "quit")
   "qq" '(save-buffers-kill-terminal :which-key "find-node")



   "r" '(:ignore t :which-key "roam")
   "rr" '(org-roam-node-find :which-key "find-node")
   "rc" '(org-roam-capture :which-key "capture")
   "ri" '(org-roam-node-insert :which-key "insert")
   "rg" '(org-roam-graph :which-key "graph")
   "rs" '(org-roam-db-sync :which-key "sync")
   "rd" '(:ignore t :which-key "dailies")
   "rdc" '(org-roam-dailies-capture-today :which-key "capture")
   "rdf" '(org-roam-dailies-find-today :which-key "find")

   ;; Todos
   "t" '(:ignore t :which-key "todos")
   "tt" '(ivy-magit-todos :which-key "goto")

   ;; Tabs
   "TAB" '(:ignore t :which-key "Tabs")
   "TAB TAB" '(tab-bar-select-tab-by-name :which-key "Goto")
   "TAB T" '(toggle-tab-bar-mode-from-frame :which-key "toggle from frame")
   "TAB d" '(tab-bar-close-tab :which-key "Delete")
   "TAB c" '(tab-bar-new-tab :which-key "Create")
   "TAB n" '(tab-bar-new-tab :which-key "New")
   "TAB l" '(tab-bar-switch-to-next-tab :which-key "Next")
   "TAB h" '(tab-bar-switch-to-prev-tab :which-key "Previous")
   "TAB r" '(tab-bar-rename-tab :which-key "Rename")
   "TAB L" '(tab-bar-move-tab :which-key "Move Right")

   ;; UI
   "u" '(:ignore t :which-key "ui")
   "ud" '(day :which-key "Day Theme")
   "uD" '(dusk :which-key "Dusk Theme")
   "ue" '(evening :which-key "Evening Theme")
   "un" '(night :which-key "Night Theme")
   "us" '(hydra-text-scale/body :which-key "scale text")
   "ut" '(:ignore t :which-key "toggle")
   "uT" '(counsel-load-theme :which-key "Theme")
   "utt" '(toggle-transparency :which-key "toggle transparency")
   "utm" '(hide-mode-line-mode :which-key "toggle mode line")
   "utM" '(toggle-menu-bar-from-frame :which-key "toggle menu bar")
   "uu" '(toggle-dark-mode :which-key "toggle dark mode")

   ;; Windows
   "w" '(:ignore t :which-key "window")
   "wd" '(evil-window-delete :which-key "delete")
   "wD" '(ace-delete-window :which-key "Delete")
   "wh" '(evil-window-left :which-key "left")
   "wn" '(evil-window-new :which-key "new")
   "wj" '(evil-window-down :which-key "down")
   "wk" '(evil-window-up :which-key "up")
   "wl" '(evil-window-right :which-key "right")
   "wm" '(maximize-window :which-key "maximize")
   "wM" '(minimize-window :which-key "minimize")
   ;; "wp" '(:ignore t :which-key "perspective")
   "wp" '(persp-switch :which-key "switch")
   "wu" '(winner-undo :which-key "winner-undo")
   "wv" '(evil-window-vsplit :which-key "vsplit")
   "wr" '(winner-redo :which-key "winner-redo")
   "ws" '(evil-window-split :which-key "split")
   "wS" '(ace-swap-window :which-key "swap")
   "ww" '(ace-window :which-key "window")
   "w-" '(evil-window-split :which-key "split")
   "w|" '(evil-window-vsplit :which-key "vsplit")
   "w|" '(balance-windows :which-key "balance")
   "w>" '(hydra-window-resize/body :which-key "hydra")

   ;; Snippets
   "y" '(:ignore t :which-key "yasnippets")
   "yy" '(yas-insert-snippet :which-key "insert snippet")
   "yc" '(yas-new-snippet :which-key "new snippet")
   "ye" '(yas-visit-snippet-file :which-key "new snippet")

   ;; Folds
   "z" '(:ignore t :which-key "folds")
   "zz" '(origami-toggle-node :which-key "toggle fold")
   "za" '(origami-toggle-node :which-key "toggle fold")
   "zj" '(origami-next-fold :which-key "next fold")
   "zk" '(origami-previous-fold :which-key "previous fold")
   "zn" '(origami-next-fold :which-key "next fold")
   "zp" '(origami-previous-fold :which-key "previous fold")
   "zr" '(origami-open-all-nodes :which-key "open all folds")
   "zm" '(origami-close-all-nodes :which-key "close all folds")
   "zM" '(hs-hide-level :which-key "close all at level")
   )

  (misenplace/leader-keys ;; apply evil-normal to pdf-view?????
     :keymaps '(global-map evil-normal-state-map pdf-view-mode-map treemacs-mode-map))
  )
(use-package git-link
  :straight t
  )
(use-package git-messenger
  :straight t
  )
(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode t)

  ;; If you would like to use git-gutter.el and linum-mode
  ;;(git-gutter:linum-setup)
  )
(use-package go-mode
  :straight t
  )
(use-package ghub
  :straight t
  )
(use-package gumshoe
  ;; :disabled
  :straight (gumshoe :type git
                     :host github
                     :repo "Overdr0ne/gumshoe"
                     :branch "master")
  :after (:all perspective persp-projectile)
  :config
  ;; (global-gumshoe-mode +1)
  (global-gumshoe-persp-mode +1)
  (setf gumshoe-slot-schema '(perspective time buffer position line))
  (setq gumshoe-show-footprints-p nil)
  ;; define a command for autocompletion of the gumshoe--global log if you’d like:
  ;; (defun consult-gumshoe-global ()
  ;;   "List global gumshoes in consult"
  ;;   (interactive)
  ;;   (consult-global-mark (ring-elements (oref gumshoe--global-backlog log))))
  ;; ;; Similarly, for the persp local gumshoe--persp log, assuming perspectives is installed:
  ;; (defun consult-gumshoe-persp ()
  ;;   "List perspective gumshoes in consult"
  ;;   (interactive)
  ;;   (consult-global-mark (ring-elements (oref gumshoe--persp-backlog log))))
  ;; ;; Similarly, for the buffer local gumshoe--persp log:
  ;; (defun consult-gumshoe-buf ()
  ;;   "List buffer gumshoes in consult"
  ;;   (interactive)
  ;;   (consult-global-mark (ring-elements (oref gumshoe--buf-backlog log))))
  )
(use-package haskell-mode
  :straight t
  )
(use-package hide-mode-line
  :straight t
  :config
  (global-hide-mode-line-mode t)
  )
;; (use-package hideshowvis ;; Would like this to work with origami-mode...
;;   :straight t
;;   :disabled
;; )
(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode t)
)
(use-package helm
  :straight t
  :config
  (require 'helm-config)
  (setq helm-input-idle-delay                     0.01
        helm-reuse-last-window-split-state        t
        helm-always-two-windows                   t
        helm-split-window-inside-p                nil
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-dedicated-window      t
        helm-frame-background-color               "DarkSlateGray"
        helm-show-action-window-other-window      'left
        helm-allow-mouse                          t
        helm-move-to-line-cycle-in-source         t
        helm-autoresize-max-height                80 ; it is %.
        helm-autoresize-min-height                20 ; it is %.
        helm-debug-root-directory                 "/home/jordan/tmp/helm-debug"
        helm-follow-mode-persistent               t
        helm-candidate-number-limit               500
        helm-visible-mark-prefix                  "✓")
  (set-face-foreground 'helm-mark-prefix "Gold1")
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume))
(use-package helm-ag
  :straight t
  :config
  (custom-set-variables
 '(helm-ag-base-command "rg --no-heading")
 `(helm-ag-success-exit-status '(0 2))))
(use-package helm-lsp
  :straight t
)

(use-package helm-swoop
  :straight t
  )
(use-package helm-gtags
  :straight t
  )
(use-package hydra
  :straight t
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("q" nil "quit" :exit t))

  (defhydra hydra-window-resize (:timeout 4)
    "scale text"
    ("j" evil-window-increase-height "inc-height")
    ("k" evil-window-decrease-height "dec-height")
    ("l" evil-window-increase-width "inc-width")
    ("h" evil-window-decrease-width "dec-width")
    ("q" nil "quit" :exit t))


  ;; (misenplace/leader-keys
  ;;   "xs" '(hydra-text-scale/body :which-key "scale text"))
  )
(use-package imenu-list
  :straight t
  )
;; (use-package interaction-log
;;   :straight t
;;   )
(use-package irony
  :straight t
  :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  )
(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode)
  )
(use-package ivy-rich
  :straight t
  :after counsel
  :config (ivy-rich-mode 1))
(use-package ivy-rtags
  :straight t
  )
(use-package julia-mode
  :straight t
  )
(use-package jupyter
  :straight t
  )
(use-package kotlin-mode
  :straight t
  )
(use-package keycast
  :straight t
  :config
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast-mode-line-update t)
        (remove-hook 'pre-command-hook 'keycast-mode-line-update)))

  (add-to-list 'global-mode-string '("" mode-line-keycast " "))) ;; TODO: Figure out why keycast package doesn't work
(use-package langtool
  :straight t
  :config
  (setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-mother-tongue "en")
)
(use-package link-hint
  :straight t
)
;; (use-package lispy
;;   :straight t
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)))
(use-package lispyville
  :straight t
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                              additional-movement slurp/barf-cp
                              prettify)))
(use-package lorem-ipsum
  :straight t
  )
(use-package lsp-docker
  :straight t
  :config
  (defvar lsp-docker-client-packages
    '(lsp-css lsp-clients lsp-bash lsp-go lsp-html lsp-pylsp ;; lsp-pyls
	      lsp-dockerfile lsp-javascript lsp-terraform))

  (setq lsp-docker-client-configs
	'((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
	  (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
	  (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
	  (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
	  (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")
	  ;; (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")
	  (:server-id pylsp :docker-server-id pylsp-docker :server-command "pylsp" :docker-image-id "martini97/lsp-docker-langservers" :docker-container-name "martini97-lsp-pylsp")
	  (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")))

  ;; (let ((projects-dir (expand-file-name "~/Projects")))
  ;;   (lsp-docker-init-clients
  ;;     :path-mappings `((,projects-dir . "/projects"))
  ;;     :client-packages lsp-docker-client-packages
  ;;     :client-configs lsp-docker-client-configs))
  )
(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         (kotlin-mode . lsp)
         (python-mode . lsp)
         (ess-r-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lua-mode
  :straight t
  )
(use-package lsp-latex
  :straight t
  :config
  (with-eval-after-load "tex-mode"
    (add-hook 'tex-mode-hook 'lsp)
    (add-hook 'latex-mode-hook 'lsp))
  ;; For YaTeX
  (with-eval-after-load "yatex"
    (add-hook 'yatex-mode-hook 'lsp))

  ;; For bibtex
  (with-eval-after-load "bibtex"
    (add-hook 'bibtex-mode-hook 'lsp))
)
(use-package lsp-ivy
  :straight t
  :commands
  lsp-ivy-workspace-symbol)
(use-package lsp-java ;; TODO: fix lsp for java
  ;; :after lsp
  :straight t
  :init
    ;; (setq lsp-java-java-path "/usr/lib/jvm/default/bin/java")
    (setq lsp-java-java-path "/usr/lib/jvm/java-18-jdk/bin/java")
    ;; (setenv "JAVA_HOME"  "/usr/bin/java")
    ;; (setq lsp-java-workspace-dir "~/work/navy/JSAPPHIRE/")
    (setq lsp-java-import-gradle-java-home "/usr/lib/jvm/java-18-jdk/bin/java")
    ;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-18" :path "usr/lib/jvm/default/bin/java" :default t)])

  ;; (setq lsp-java-configuration-runtimes '[(:name "Default" :path "/usr/lib/jvm/default-runtime/" :default t)
  ;; 					  (:name "Java-18-openjdk" :path "/usr/lib/jvm/java-18-openjdk/")])

    ;; (setq lsp-java-vmargs (list "-noverify" "--enable-preview"))
  :config
  ;; Enable dap-java
  (require 'dap-java)

  ;; Support Lombok in our projects, among other things
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" jmi/lombok-jar)
              (concat "-Xbootclasspath/a:" jmi/lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil
	)

        ;; Formatter profile
        ;; lsp-java-format-settings-url
        ;; (concat "file://" jmi/java-format-settings-file))
    ;; (setq-default lsp-java-import-gradle-enabled t)
    ;; (setq-default lsp-java-import-maven-enabled f)
    (add-hook 'java-mode-hook #'lsp)
    ;; (setq max-lisp-eval-depth 10000)
    ;; (setq lsp-java-imports-gradle-wrapper-checksums [(:sha256 "e2b82129ab64751fd40437007bd2f7f2afb3c6e41a9198e628650b22d5824a14" :allowed true)])
    ;; (add-hook 'java-mode-hook 'lsp)
    ;; (progn (add-hook 'java-mode-hook 'lsp)
    ;; 	   (require 'dap-java))
    ;; (setq lsp-java-gradle-version "7.4.2")
    ;; (setq lsp-java-import-gradle-wrapper-enabled f)

    (dap-register-debug-template "Runner"
				(list :type "java"
				    :request "launch"
				    :args ""
				    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
				    :projectName "myapp"
				    :mainClass "edu.msu.utilities.LinSpace"
				    :env '(("DEV" . "1"))))
)
(use-package dap-java :ensure nil)
(use-package lsp-ui
  :straight t
  )
;; (use-package lsp-latex
;;   :commands
;;   (with-eval-after-load "tex-mode"
;;     (add-hook 'tex-mode-hook 'lsp)
;;     (add-hook 'latex-mode-hook 'lsp)
;;     (add-hook 'LaTeX-mode-hook 'lsp))
;;
;;   ;; For YaTeX
;;   (with-eval-after-load "yatex"
;;     (add-hook 'yatex-mode-hook 'lsp))
;;
;;   ;; For bibtex
;;   (with-eval-after-load "bibtex"
;;     (add-hook 'bibtex-mode-hook 'lsp)))
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list
  )
(use-package magit
  :straight t
  )
(use-package magit-todos
  :straight t
  )
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
;; (use-package magithub
;;   :straight t
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/git_repos"))
;; (use-package mpc
;;   :straight t
;;   )
;; (use-package mu4e
;;   :straight t
;;   :config
;; 
;;   (setq mu4e-change-filenames-when-moving t)
;;   (setq mu4e-update-interval (* 10 60))
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   (setq mu4e-maildir "~/Mail")
;;   (setq mu4e-drafts-folder "/[Gmail].Drafts")
;;   (setq mu4e-sent-folder "/[Gmail].Drafts")
;;   (setq mu4e-refile-folder "/[Gmail].All Mail")
;;   (setq mu4e-trash-folder "/[Gmail].Trash")
;;   )
(use-package multi-vterm
  :straight t
  :config
  )
(use-package native-complete
  :straight (:host github :repo "CeleritasCelery/emacs-native-shell-complete")
  :config
   (with-eval-after-load 'shell
     (native-complete-setup-bash))
   )
(use-package nyan-mode
  :straight t
  :config
  (nyan-mode 1))
(use-package olivetti
  :straight t
  )
(use-package ob-mermaid
  :straight t
  :after org
  :config
  (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc"))
(use-package ob-async
  :straight t
  )
;; (use-package omnisharp-emacs
;;   :straight (:host github :repo "OmniSharp/omnisharp-emacs")
;;   )
(use-package org
  :straight t
  :config

  (defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun org-latex-fragment-toggle ()
  "Toggle a latex fragment image "
  (and (eq 'org-mode major-mode)
       (let* ((el (org-element-context))
              (el-type (car el)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-latex-fragment-last
            ;; and are on a fragment now
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type))
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (= (org-element-property :begin el)
                    (org-element-property :begin org-latex-fragment-last))))
           ;; go back to last one and put image back
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; now remove current image
           (goto-char (org-element-property :begin el))
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           ;; and save new fragment
           (setq org-latex-fragment-last el))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type)))
            ;; but we were on one
            org-latex-fragment-last)
           ;; put image back on
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; unset last fragment
           (setq org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; but now we are
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type)))
           (goto-char (org-element-property :begin el))
           ;; remove image
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))


(add-hook 'post-command-hook 'org-latex-fragment-toggle)










  ;; Set org-mode for .org files
(setq org-format-latex-options '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
	     ("begin" "$1" "$" "$$" "\\(" "\\[")))


  (setq auto-mode-alist (cons '("\\.org" . org-mode) auto-mode-alist))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Org file location settigns
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "/todo.org"))
  (setq org-agenda-files (list org-directory))

  ;; Org-agenda settings
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)

  ;; Org todo keywords
  (setq org-todo-keywords
        '((sequence "PIPELINE"
                    "NEXT"
                    "TODO"
                    "DONE")))

  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only t)

  ;; Org-babel
  (setq org-babel-python-command "/usr/local/bin/python")
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . nil)
      (mermaid . t)
      (shell . t)
      (python . t)
      (scheme . t)
      ;; (julia . t)
      (C . t)
      (R . t)))

  ;; Latex
   (setq org-latex-pdf-process
 	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
 	"bibtex %b"
 	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
 	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        )

  (setq org-confirm-babel-evaluate nil)
  (setq org-latex-image-default-width "")
  (setq org-latex-classes
	'(("article"
	   "\\documentclass[11pt]{article}"
	   ("\\section{%s}" . "\\section*{%s}")
	   ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	  ("report"
           "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	  ("book"
           "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	  ("beamer"
           "\\documentclass\[presentation\]\{beamer\}"
           ("\\section\{%s\}" . "\\section*\{%s\}")
           ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
           ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))
	  ("IEEEtran"
           "\\documentclass{IEEEtran}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	  ))

  (setq org-latex-listings t)

  ;; Org for writing a journal
  (defvar org-journal-file "~/org/journal.org"
     "Path to OrgMode journal file.")
  (defvar org-journal-date-format "%Y-%m-%d"
     "Date format string for journal headings.")
  (defun org-journal-entry ()
    "Create a new diary entry for today or append to an existing one."
    (interactive)
    (switch-to-buffer (find-file org-journal-file))
    (widen)
    (let ((today (format-time-string org-journal-date-format)))
      (beginning-of-buffer)
      (unless (org-goto-local-search-headings today nil t)
        ((lambda ()
           (org-insert-heading)
           (insert today)
           (insert "\n\n  \n"))))
      (beginning-of-buffer)
      (org-show-entry)
      (org-narrow-to-subtree)
      (end-of-buffer)
      (backward-char 2)
      (unless (= (current-column) 2)
        (insert "\n\n  "))))

  ;; TODO: work on org-capture templates
  (setq org-capture-templates
	'(("a" "Appointment" entry (file "~/org/schedule.org")
	   "* %? \n\n:Properties:\n:calendar-id: jordans1882@gmail.com\n:LOCATION:\n:END:\n\n:org-gcal:\n%^T\n:END:\n\n")
	  ("i" "inbox" entry (file "~/org/inbox.org")
	   "* TODO %?")
	  ))
  )
(use-package org-contrib
  :straight t
  )
;; (use-package org-plus-contrib
;;   :straight t
;;   )
(use-package org-auto-tangle
  :straight t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))
(use-package org-gantt-mode
  :straight (org-gantt-mode :type git
                            :host gitlab
                            :repo "joukeHijlkema/org-gantt"
                            :branch "master")
  )
;; (use-package org-gantt
;;   :straight (org-gantt :type git
;;                        :host github
;;                        :repo "SeungukShin/org-gantt"
;;                        :branch "master")
;;   )
(use-package ox-reveal
  :straight t
  :config
  (setq org-reveal-root "file:///home/jordan/git_repos/reveal.js/"))
;; (use-package ox-latex
;;   :straight t
;;   :config
;;   (add-to-list 'org-latex-packages-alist '("" "minted"))
;;   (setq org-latex-listings 'minted)
;;   (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
(use-package org-ref
  :straight t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq reftex-default-bibliography '("~/research/dissertation/main.bib"))

  ;; see org-ref for use of these variables
  (setq bibtex-completion-bibliography '("~/org/ref/references.bib"
					 "~/research/dissertation/main.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
     org-ref-default-bibliography '("~/research/dissertation/main.bib")
     org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

  )
(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )
(use-package org-noter
  :straight t
  )
(use-package org-pomodoro
  :straight t
  )
(use-package org-preview-html
  :straight t
  )
(use-package org-projectile
  :straight t
  :after org
  :preface
      (setq org-roam-v2-ack t)
  :bind (
	 ;; ("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath "todo.org")
    ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)

    )

  )
(use-package org-roam
      :straight t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "~/org/org-roam/"))
      (org-roam-dailies-directory (file-truename "~/org/org-roam-daily/"))
      :config

    (defun jab/search-roam ()
	"Run consult-ripgrep on the org roam directory"
	(interactive)
	(consult-ripgrep org-roam-directory nil))


      (org-roam-db-autosync-mode)

      (setq org-roam-capture-templates
	    '(("d" "default" plain "%?" :target
	       (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
	       :unnarrowed t)
	      ("r" "ref" plain "%?"
	       :target (file+head "refs/${citekey}.org"
				  "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
	       :unnarrowed t)
	      ))

      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry
               #'org-roam-capture--get-point
               "* %?"
               :file-name "org-roam-daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n\n"
               :olp ("Misc notes"))
          ("r" "ref" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              ,(s-join "\n" (list "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n"
                                                  "Title:\n"
                                                  "Year:\n"
                                                  "Authors:\n"
                                                  "Location:\n"
                                                  "* Reading notes:\n"
                                                  "* Initial thoughts:\n"
                                                  "* High-level summary:\n"
                                                  "* Quick-notes:\n"
                                                  "* In-depth notes:\n"
                                                  "* Quotes:\n")))
           :unnarrowed t)
          ("p" "ref + physical" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n* Notes :physical:")
           :unnarrowed t)
          ("n" "ref + noter" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              ,(s-join "\n" (list "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n"
                                                  "* Notes :noter:"
                                                  ":PROPERTIES:"
                                                  ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                                                  ":NOTER_PAGE:"
                                                  ":END:")))
           :unnarrowed t)

               )))
(use-package org-roam-bibtex
  :straight t
  )
;; (use-package org-roam-server
;;   :straight t
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))
(use-package org-sidebar
  :straight t
  )
(use-package org-super-agenda
  :straight t
  :init (org-super-agenda-mode t)
  :config
  (setq org-agenda-custom-commands
	'(

("d" "Dissertation view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Dissertation"
                                 :tag "dissertation"
                                 :order 20)
                          (:discard (:tag ("emacs" "general" "home" "inbox" "navy" "qubbd")))))))))

("e" "Emacs view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 20)
                          (:discard (:tag ("dissertation" "general" "home" "inbox" "navy" "qubbd")))))))))

("h" "Home view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Chores"
                                 :tag "chores"
                                 :order 18)
                          (:name "In-town Shopping"
                                 :tag "intown_shopping"
                                 :order 18)
                          (:name "Online Shopping"
                                 :tag "online_shopping"
                                 :order 18)
                          (:name "Food Prep"
                                 :tag "food_prep"
                                 :order 19)
                          (:name "Errands"
                                 :tag "errands"
                                 :order 19)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 20)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:discard (:tag ("dissertation" "navy" "qubbd")))))))))


("i" "Inbox view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Inbox"
                                 :tag "inbox"
                                 :order 20)
                          (:discard (:tag ("dissertation" "general" "home" "inbox" "navy" "qubbd" "Emacs")))))))))


("n" "Navy view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Navy"
                                 :tag "navy"
                                 :order 20)
                          (:discard (:tag ("emacs" "dissertation" "general" "home" "inbox" "navy" "qubbd")))))))))



("w" "Work view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "General"
                                 :tag "general"
                                 :order 10)
                          (:name "Dissertation"
                                 :tag "dissertation"
                                 :order 15)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("home" "Emacs")))))))))


("z" "Super zaen view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "General"
                                 :tag "general"
                                 :order 10)
                          (:name "Dissertation"
                                 :tag "dissertation"
                                 :order 15)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 19)
                          (:name "Chores"
                                 :tag "chores"
                                 :order 18)
                          (:name "In-town Shopping"
                                 :tag "intown_shopping"
                                 :order 18)
                          (:name "Online Shopping"
                                 :tag "online_shopping"
                                 :order 18)
                          (:name "Food Prep"
                                 :tag "food_prep"
                                 :order 19)
                          (:name "Errands"
                                 :tag "errands"
                                 :order 19)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))

	))
)
(use-package org-transclusion
  :straight (:host github :repo "nobiot/org-transclusion"))
;; (use-package orderless
;;   :straight t
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))
(use-package origami
  :straight t
  )
;; (use-package paredit
;;   :straight t
;;   )
(use-package perspective
  :straight t
  :after projectile counsel-projectile
  :config
  (persp-mode)
  )
(use-package persp-projectile
  :straight t
  :after perspective, projectile, counsel-projectile
  )
(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config

  (defun enable-pdf-view-themed-minor-mode-hook ()
    (pdf-view-themed-minor-mode 1))


  (add-hook 'pdf-view-mode-hook 'disable-display-line-numbers-mode-hook)
  (add-hook 'pdf-view-mode-hook 'enable-pdf-view-themed-minor-mode-hook)

  (evil-make-overriding-map pdf-view-mode-map 'normal)
  (evil-define-key 'normal pdf-view-mode-map
  "h" 'image-backward-hscroll
  "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
  "l" 'image-forward-hscroll
  "G" 'pdf-view-last-page)

  ;; (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  ;; (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
  ;; (define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
  ;; (define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)
  ;; (define-key doc-view-mode-map (kbd "gg") 'doc-view-first-page)
  ;; (define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)
  ;; (define-key doc-view-mode-map (kbd "C-w C-w") 'evil-window-next)
  ;; (define-key doc-view-mode-map (kbd "C-w C-w") 'evil-window-next)
  ;; (define-key doc-view-mode-map (kbd "-") 'doc-view-shrink)
  ;; (define-key doc-view-mode-map (kbd "+") 'doc-view-enlarge)
  ;; ;; (define-key doc-view-mode-map "," my-leader-map)
  ;; (define-key doc-view-mode-map "M-h" 'evil-window-left)
  ;; (define-key doc-view-mode-map "M-l" 'evil-window-right)
  ;; (define-key doc-view-mode-map "M-j" 'evil-window-down)
  ;; (define-key doc-view-mode-map "M-k" 'evil-window-up)
  )
(use-package polymode
  :straight t)
(use-package poly-markdown
  :straight t)
(use-package poly-R
  :straight t)
(use-package poly-org
  :straight t)
(use-package pomodoro
  :straight t
  :config
    (pomodoro-add-to-mode-line))
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode)
  )
(use-package projectile
  :straight t
  :config
  (projectile-mode)
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'ivy)
  (defun projectile-test-suffix (project-type) ;; tmi: expand these for more languages (C++,python, etc.)
    "Find default test files suffix based on PROJECT-TYPE."
    (cond
     ((member project-type '(rails-rspec ruby-rspec)) "_spec")
     ((member project-type '(rails-test ruby-test lein-test go)) "_test")
     ((member project-type '(r)) "_test")
     ((member project-type '(scons)) "test")
     ((member project-type '(maven symfony)) "Test")
     ((member project-type '(gradle grails)) "Spec")))


  )
(use-package pyvenv
  :straight t
  :config
    (setenv "home" (expand-file-name "~/anaconda3/envs"))
    (pyvenv-mode 1)
    (pyvenv-activate "base")
  )
(use-package pycoverage
  :straight t
  )
(use-package python-pytest
  :straight t
  )
(use-package rainbow-delimiters
  :straight t
  )
(use-package rainbow-mode
  :straight t
  :config
  (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (my-global-rainbow-mode 1)
  )
(use-package racket-mode
  :straight t
  )
(use-package ranger
  :straight t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t))
(use-package restart-emacs
  :straight t
  )
(use-package rtags
  :straight t
  )
(use-package rust-mode
  :straight t
  :enusre t
  :after lsp lsp-rust
  :hook ((rust-mode . flycheck-mode)
	 (rust-mode . lsp-deferred))
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'lsp)
  )
(use-package skeletor
  :straight t
  )
(use-package sublimity
  :disabled ;; Doesn't seem to work... but who cares anyway
  :straight t
  )
(use-package swiper
  :straight t
  )
(use-package vertico
  :straight t
  :init
  (vertico-mode)
)
(use-package theme-magic
  :straight t
  :config
  (theme-magic-export-theme-mode)
  )
(use-package treemacs
  :straight t
  :after perspective
  :init
  (defvar treemacs-no-load-time-warnings t)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
;; (use-package treemacs-evil
;;   :straight t
;;   :after (:all treemacs evil)
;;   )
(use-package treemacs-projectile
  :straight t
  :after (:all treemacs projectile)
  )
(use-package treemacs-perspective
  :straight t
  :after (:all treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives)
  )
(use-package treemacs-magit
  :straight t
  :after (:all treemacs magit)
  )
(use-package tree-sitter
  :straight t
  )
(use-package tree-sitter-langs
  :straight t
  )

;; MAYBE: add switch for arch linux?
;; (use-package webkit
;;   :straight (:type git :host github :repo "akirakyle/emacs-webkit"
;;              :branch "main"
;;              :files (:defaults "*.js" "*.css" "*.so")
;;              :pre-build ("make"))
;;   )
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  )
(use-package winner
  :straight t
  :config
  (winner-mode))
(use-package ws-butler
  :straight t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))
(use-package wrap-region
  :straight t
  :config
  (wrap-region-mode t))
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)

  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
  ;;(setq yas-snippet-dirs '("~/git_repos/misenplace-snippets"))
    )
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package stan-mode
  :straight t
  )
(use-package visual-regexp-steroids
  :straight t
  )
(use-package vterm
  :straight t
  :config
  (add-hook 'vterm-mode-hook 'disable-display-line-numbers-mode-hook)
(setq display-line-numbers-mode nil)
  )
;; (use-package weatherline-mode
;;   :straight (:host github :repo "aaron-em/weatherline-mode.el")
;;   )

;; (add-hook 'after-init-hook 'org-agenda-list)

;; (add-hook 'after-init-hook '(lambda () (org-agenda-list 1)))

;; (setq inhibit-splash-screen t)
;; (org-agenda-list)
;; (delete-other-windows)

(if (file-exists-p "~/.emacs.d/personal-configs.el")
    (load-file "~/.emacs.d/personal-configs.el"))

;; (eval-after-load
;; 'company
;; '(add-to-list 'company-backends #'company-omnisharp))

;; (setq org-agenda-files (append org-agenda-files (mapcar (lambda (s) (concat s "todo.org")) projectile-known-projects)))
;; (setq org-agenda-files (remove-duplicates (append org-agenda-files projectile-known-projects)))
(setq org-agenda-files projectile-known-projects)
(setq counsel-projectile-org-capture-templates '(("t" "[${name}] Todos" entry
						  (file+headline "${root}/todo.org" "Todos")
						  "*** TODO %?\n%u\n%a")))
(setq require-final-newline nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-base-command "rg --no-heading")
 '(helm-ag-success-exit-status '(0 2))
 '(helm-minibuffer-history-key "M-p")
 '(safe-local-variable-values '((eval hs-minor-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar ((t (:background "dim gray" :foreground "#1d2021" :box nil))))
 '(tab-bar-tab ((t (:background "#100011" :foreground "#c5d4dd" :box nil)))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") )


(if (file-exists-p "~/.emacs.d/ess-vterm.el")
    (load-file "~/.emacs.d/ess-vterm.el"))

(add-hook 'ess-r-mode-hook #'r-vterm-mode)

(load-file "~/.emacs.d/hide-comnt.el")

;; (add-to-list 'load-path "~/.emacs.d/")
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)



;; Local Variables:
;; mode: lisp-interaction
;; eval: (hs-minor-mode t)
;; End:
