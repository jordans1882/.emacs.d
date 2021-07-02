;; Misenplace:
;; My emacs config

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

;; Use packages with configs

(use-package ace-jump-mode
  :straight t
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
(use-package auctex-latexmk
  :straight t
  :config
  (auctex-latexmk-setup))
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
(use-package cmake-ide
  :straight t
  :config
  (cmake-ide-setup))
(use-package command-log-mode
  :straight t
  )
(use-package company
  :straight t
  :config
  ;;(add-to-list 'auto-mode-alist '("\\.h\\'" . company-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.hpp\\'" . company-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.cpp\\'" . company-mode))
  )
(use-package company-irony
  :straight t
  )
(use-package conda
  :straight t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  ;;(setq conda-env-home-directory (expand-file-name "~/anaconda3/envs"))
  )
(use-package counsel
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
  (setq projectile-indexing-method 'native))
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/_assets/mise_en_place.png"))
(use-package dockerfile-mode
  :straight t
  )
(use-package doom-modeline
      :straight t
      :hook (after-init . doom-modeline-mode))
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package elpy
  :straight t
  :init
  (elpy-enable))
(use-package ess
  :straight t
  :config
  (setq ess-use-flymake nil) ;; disable Flymake
  (add-hook 'ess-mode-hook '(lambda () (define-key ess-mode-map (kbd "M-<RET>") 'ess-eval-region-or-line-visibly-and-step)))
  (add-hook 'ess-mode-hook '(lambda () (define-key ess-mode-map (kbd "C-S-<RET>") 'ess-eval-region-or-function-or-paragraph-and-step)))
  )
(use-package emacs
  :config
    ;; Appearance
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (set-frame-parameter (selected-frame) 'alpha '(95 50))
    (add-to-list 'default-frame-alist '(alpha 95 50))
    (tool-bar-mode -1)

    (setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

    ;; Utility funs
    (defun dawn ()
	"Set theme to day"
	(interactive)
	(counsel-load-theme-action "doom-gruvbox-light"))
    (defun day ()
	"Set theme to day"
	(interactive)
	(counsel-load-theme-action "doom-solarized-light"))
    (defun sunny ()
	"Set theme to day"
	(interactive)
	(counsel-load-theme-action "doom-nord-light"))
    (defun dusk ()
	"Set theme to dusk"
	(interactive)
	(counsel-load-theme-action "doom-nova"))
    (defun evening ()
	"Set theme to evening"
	(interactive)
	(counsel-load-theme-action "doom-gruvbox"))
    (defun late-night ()
	"Set theme to night"
	(interactive)
	(counsel-load-theme-action "doom-Iosvkem"))
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
   (define-key evil-normal-state-map (kbd "/") 'swiper)
   (define-key evil-normal-state-map (kbd "?") 'swiper-backward)

   ;; Manually add in my-leader-map bindings to states
   ;; (define-key compilation-mode-map "," my-leader-map)
   ;; (define-key compilation-mode-map " " my-second-leader-map)
   )
(use-package evil-collection
  :straight t
  :config
  (evil-collection-init)
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
(use-package lua-mode
  :straight t
  )
(use-package magit
  :straight t
  )
(use-package nyan-mode
  :straight t
  :config
  (nyan-mode 1))
(use-package polymode
  :straight t)
(use-package poly-markdown
  :straight t)
(use-package poly-R
  :straight t)
(use-package poly-org
  :straight t)
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode)
  )
(use-package projectile
  :straight t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (defun projectile-test-suffix (project-type)
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
  )
(use-package python-pytest
  :straight t
  )
(use-package ob-mermaid
  :straight t
  :config
  (setq ob-mermaid-cli-path "~/mermaid/node_modules/.bin/mmdc"))
(use-package org
  :straight t
  :config
  ;; Set org-mode for .org files
  (setq auto-mode-alist (cons '("\\.org" . org-mode) auto-mode-alist))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Org file location settigns
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "/todo.org"))

  ;; Org-agenda settings
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)

  ;; Org todo keywords
  (setq org-todo-keywords
        '((sequence "PIPELINE"
                    "TODO"
                    "DONE")))

  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only t)

  ;; Org-babel
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . nil)
      (shell . t)
      (python . t)
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


  )
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
  (setq reftex-default-bibliography '("~/git_repos/phd_comps2/js_phd_comp.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
     org-ref-default-bibliography '("~/git_repos/phd_comps2/js_phd_comp.bib")
     org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

  )

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )
(use-package org-pomodoro
  :straight t
  )
(use-package org-preview-html
  :straight t
  )
(use-package org-projectile
  :straight t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath "todo.org")
    ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)))
(use-package org-roam
      :straight t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "/home/jordan/org-roam/"))
      (org-roam-dailies-directory (file-truename "/home/jordan/org-roam-daily/"))
      :config
      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry
               #'org-roam-capture--get-point
               "* %?"
               :file-name "org-roam-daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n\n"
               :olp ("Misc notes"))

              ("l" "lab" entry
                #'org-roam-capture--get-point
                "* %?"
                :file-name "org-roam-daily/%<%Y-%m-%d>"
                :head "#+title: %<%Y-%m-%d>\n"
                :olp ("Lab notes"))

               ("j" "journal" entry
                #'org-roam-capture--get-point
                "* %?"
                :file-name "org-roam-daily/%<%Y-%m-%d>"
                :head "#+title: %<%Y-%m-%d>\n"
                :olp ("Journal"))))

      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
;; (use-package orderless
;;   :straight t
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))
(use-package ranger
  :straight t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t))
(use-package swiper
  :straight t
  )
;; (use-package vertico
;;   :straight t
;;   :init
;;   (vertico-mode)
;; )
(use-package which-key
  :straight t
  :config
  (which-key-mode)

  ;; binding "SPC-t" for toggles
  (define-key my-second-leader-map "t" '("agenda-prefix"))
  ;; toggle theme
  (define-key my-second-leader-map "ttt" 'toggle-transparency)
  (define-key my-second-leader-map "ttd" 'day)
  (define-key my-second-leader-map "ttD" 'dusk)
  (define-key my-second-leader-map "tte" 'evening)
  (define-key my-second-leader-map "ttn" 'night)
  ;; toggle modelines
  (define-key my-second-leader-map "tM" 'toggle-menu-bar-mode-from-frame)
  (define-key my-second-leader-map "tm" 'hide-mode-line-mode)

  ;; binding for comments
  (define-key my-second-leader-map "c" '("comment-prefix"))
  (define-key my-second-leader-map "cc" 'evilnc-comment-or-uncomment-lines)

  ;; binding ",a" for agenda (todo manager)
  (define-key my-leader-map "a" '("agenda-prefix"))
  (define-key my-leader-map "aa" 'org-agenda)
  (define-key my-leader-map "as" 'org-schedule)
  (define-key my-leader-map "an" 'org-projectile-capture-for-current-project)
  (define-key my-leader-map "acc" 'counsel-org-clock-goto)
  (define-key my-leader-map "aci" 'org-clock-in)
  (define-key my-leader-map "ach" 'counsel-org-clock-history)
  (define-key my-leader-map "aco" 'org-clock-out)
  (define-key my-leader-map "agp" 'org-projectile-goto-location-for-project)
  (define-key my-leader-map "at" 'org-todo)
  (define-key my-leader-map "ap" 'org-pomodoro)

  ;; binding ",b" for buffers
  (define-key my-leader-map "b" '("buffer-prefix"))
  (define-key my-leader-map "bb" 'switch-to-buffer)
  (define-key my-leader-map "bl" 'next-buffer)
  (define-key my-leader-map "bh" 'previous-buffer)
  (define-key my-leader-map "bc" 'evil-buffer-new)
  (define-key my-leader-map "bn" 'evil-next-buffer)
  (define-key my-leader-map "bp" 'evil-prev-buffer)
  (define-key my-leader-map "bd" 'evil-delete-buffer)
  (define-key my-leader-map "br" 'rename-buffer)

  ;; binding ",c" for nerd commenter
  (define-key my-leader-map "c" '("comment-prefix"))
  (define-key my-leader-map "cc" 'evilnc-comment-or-uncomment-lines)

  ;; binding ",d" for dumb-jump
  (define-key my-leader-map "d" '("dumb-jump-prefix"))
  (define-key my-leader-map "dd" 'dumb-jump-go)
  (define-key my-leader-map "db" 'dumb-jump-back)
  (define-key my-leader-map "do" 'dumb-jump-go-other-window)
  (define-key my-leader-map "dp" 'dumb-jump-go-prompt)
  (define-key my-leader-map "dq" 'dumb-jump-quick-look)

  ;; binding ",e" for error (flycheck)
  (define-key my-leader-map "e" '("evilnc-prefix"))
  (define-key my-leader-map "ee" 'flycheck-list-errors)
  (define-key my-leader-map "e/" 'counsel-flycheck)
  (define-key my-leader-map "ej" 'flycheck-next-error)
  (define-key my-leader-map "ek" 'flycheck-previous-error)
  (define-key my-leader-map "en" 'flycheck-next-error)
  (define-key my-leader-map "ep" 'flycheck-previous-error)

  ;; binding " f" for files
  (define-key my-leader-map "f" '("files-prefix"))
  (define-key my-leader-map "ff" 'treemacs)
  (define-key my-leader-map "fc" 'treemacs-create-file)
  (define-key my-leader-map "fC" 'treemacs-create-dir)
  (define-key my-leader-map "fd" 'treemacs-delete)
  (define-key my-leader-map "feb" 'edit-bashrc)
  (define-key my-leader-map "fea" 'edit-awesomerc)
  (define-key my-leader-map "fee" 'edit-config)
  (define-key my-leader-map "feq" 'edit-qutebrowser)
  (define-key my-leader-map "fer" 'reload-config)
  (define-key my-leader-map "fev" 'edit-vimrc)
  (define-key my-leader-map "fey" 'edit-yas-config)
  (define-key my-leader-map "fq" 'treemacs-quit)
  (define-key my-leader-map "fs" 'treemacs-visit-node-horizontal-split)
  (define-key my-leader-map "fv" 'treemacs-visit-node-vertical-split)

  ;; binding ",g" for git
  (define-key my-leader-map "g" '("git-prefix"))
  (define-key my-leader-map "gb" 'magit-branch)
  (define-key my-leader-map "gB" 'magit-branch-create)
  (define-key my-leader-map "gc" 'magit-commit)
  (define-key my-leader-map "gg" 'magit-status)
  (define-key my-leader-map "gl" 'magit-log)
  (define-key my-leader-map "gs" 'git-gutter:stage-hunk)
  (define-key my-leader-map "gS" 'magit-stage)
  (define-key my-leader-map "gU" 'magit-unstage)
  (define-key my-leader-map "gj" 'git-gutter:next-diff)
  (define-key my-leader-map "gk" 'git-gutter:previous-diff)
  (define-key my-leader-map "gn" 'git-gutter:next-diff)
  (define-key my-leader-map "gp" 'git-gutter:previous-diff)
  (define-key my-leader-map "g>" 'magit-pull)
  (define-key my-leader-map "g<" 'magit-push)

  ;; binding ",h" for help
  (define-key my-leader-map "h" '("help-prefix"))
  (define-key my-leader-map "hm" 'describe-mode)
  (define-key my-leader-map "hf" 'describe-function)
  (define-key my-leader-map "hv" 'describe-variable)
  (define-key my-leader-map "hc" 'counsel-cheatsheets)

  ;; binding ",j" for jump
  (define-key my-leader-map "j" '("jump-prefix"))
  (define-key my-leader-map "jb" 'dumb-jump-back)
  (define-key my-leader-map "jd" 'dumb-jump-go)
  (define-key my-leader-map "jt" 'projectile-find-tag)
  (define-key my-leader-map "jj" 'evil-ace-jump-char-mode)
  (define-key my-leader-map "jw" 'evil-ace-jump-word-mode)
  (define-key my-leader-map "jl" 'evil-ace-jump-line-mode)


  ;; binding ",o" for org
  (define-key my-leader-map "o" '("org-prefix"))
  (define-key my-leader-map "occ" 'org-capture)
  (define-key my-leader-map "orc" 'org-roam-capture)
  (define-key my-leader-map "orf" 'org-roam-find-file)
  (define-key my-leader-map "ordc" 'org-roam-dailies-capture-today)
  (define-key my-leader-map "ordf" 'org-roam-dailies-find-today)
  (define-key my-leader-map "oci" 'org-clock-in)
  (define-key my-leader-map "oco" 'org-clock-out)

  ;; binding ",p" for projects
  (define-key my-leader-map "p" '("projects-prefix"))
  (define-key my-leader-map "pA" 'projectile-add-known-project)
  (define-key my-leader-map "pa" 'counsel-projectile-org-agenda)
  (define-key my-leader-map "pc" 'counsel-projectile-org-capture)
  (define-key my-leader-map "pd" 'counsel-projectile-find-dir)
  (define-key my-leader-map "pm" 'projectile-compile-project)
  (define-key my-leader-map "pp" 'counsel-projectile-switch-project)
  (define-key my-leader-map "pf" 'counsel-projectile-find-file)
  (define-key my-leader-map "pq" 'projectile-kill-buffers)
  (define-key my-leader-map "pr" 'counsel-projectile-rg)
  (define-key my-leader-map "ps" 'projectile-run-shell)
  (define-key my-leader-map "ptt" 'projectile-find-tag)
  (define-key my-leader-map "ptr" 'projectile-tag-regenerate)
  (define-key my-leader-map "ptT" 'projectile-test-project)
  (define-key my-leader-map "ptf" 'projectile-find-test-file)
  (define-key my-leader-map "p[" 'projectile-previous-project-buffer)
  (define-key my-leader-map "p]" 'projectile-next-project-buffer)

  ;; binding ",r" for R programming language
  (define-key my-leader-map "rpb" 'ess-r-devtools-build)
  (define-key my-leader-map "rpc" 'ess-r-devtools-check-package)
  (define-key my-leader-map "rpi" 'ess-r-devtools-install-package)
  (define-key my-leader-map "rpl" 'ess-r-devtools-load-package)
  (define-key my-leader-map "rpt" 'ess-r-devtools-test-package)
  (define-key my-leader-map "rh" 'ess-display-help-on-object)
  (define-key my-leader-map "ro" 'ess-rdired)
  (define-key my-leader-map "rt" 'ess-eval-structure)
  (define-key my-leader-map "ri" 'asb-ess-R-object-popup-str)
  (define-key my-leader-map "rdi" 'asb-ess-R-object-popup-str)
  (define-key my-leader-map "rdI" 'asb-ess-R-object-popup-interactive)
  (define-key my-leader-map "rdc" 'asb-ess-R-object-popup-cls)
  (define-key my-leader-map "rI" 'asb-ess-R-object-popup-interactive)
  (define-key my-leader-map "rr" 'ess-eval-region-and-go)
  (define-key my-leader-map "rq" 'ess-watch-quit)

  ;; binding ",t" for tabs
  (define-key my-leader-map "tt" 'tab-bar-select-tab-by-name)
  (define-key my-leader-map "tT" 'toggle-tab-bar-mode-from-frame)
  (define-key my-leader-map "tc" 'tab-bar-new-tab)
  (define-key my-leader-map "td" 'tab-bar-close-tab)
  (define-key my-leader-map "tl" 'tab-bar-switch-to-next-tab)
  (define-key my-leader-map "th" 'tab-bar-switch-to-prev-tab)
  (define-key my-leader-map "tr" 'tab-bar-rename-tab)
  (define-key my-leader-map "tL" 'tab-bar-move-tab)

  (which-key-add-key-based-replacements ",a" "agenda")
  (which-key-add-key-based-replacements ",b" "buffers")
  (which-key-add-key-based-replacements ",d" "dumb")
  (which-key-add-key-based-replacements ",e" "errors")
  (which-key-add-key-based-replacements ",f" "files")
  (which-key-add-key-based-replacements ",g" "git")
  (which-key-add-key-based-replacements ",h" "help")
  (which-key-add-key-based-replacements ",j" "jump")
  (which-key-add-key-based-replacements ",p" "projects")
  (which-key-add-key-based-replacements ",o" "org")
  (which-key-add-key-based-replacements ",r" "R")
  (which-key-add-key-based-replacements ",t" "tabs")
  (which-key-add-key-based-replacements ",x" "edit")
  (which-key-add-key-based-replacements ",w" "windows")
  (which-key-add-key-based-replacements ",y" "yas")

  ;; binding ",w" for windows
  (define-key my-leader-map "wd" 'evil-window-delete)
  (define-key my-leader-map "wh" 'evil-window-left)
  (define-key my-leader-map "wn" 'evil-window-new)
  (define-key my-leader-map "wj" 'evil-window-down)
  (define-key my-leader-map "wk" 'evil-window-up)
  (define-key my-leader-map "wl" 'evil-window-right)
  (define-key my-leader-map "wm" 'maximize-window)
  (define-key my-leader-map "wM" 'minimize-window)
  (define-key my-leader-map "wu" 'winner-undo)
  (define-key my-leader-map "wv" 'evil-window-vsplit)
  (define-key my-leader-map "wr" 'winner-redo)
  (define-key my-leader-map "ws" 'evil-window-split)
  (define-key my-leader-map "wJ" 'evil-window-decrease-height)
  (define-key my-leader-map "wK" 'evil-window-increase-height)
  (define-key my-leader-map "wH" 'evil-window-decrease-width)
  (define-key my-leader-map "wL" 'evil-window-increase-width)
  (define-key my-leader-map "w-" 'evil-window-split)
  (define-key my-leader-map "w|" 'evil-window-vsplit)
  (define-key my-leader-map "w=" 'balance-windows)

  ;; binding ",x" for edit commands
  (define-key my-leader-map "x" '("edit-prefix"))
  (define-key my-leader-map "xc" 'evilnc-comment-or-uncomment-lines)
  (define-key my-leader-map "xi" 'text-scale-increase)
  (define-key my-leader-map "xd" 'text-scale-decrease)

  ;; binding ",y" for yasnippets
  (define-key my-leader-map "y" '("yas-prefix"))
  (define-key my-leader-map "yy" 'yas-insert-snippet)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval hs-minor-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; mode: lisp-interaction
;; eval: (hs-minor-mode t)
;; End:
