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

(use-package counsel
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
(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  )
(use-package ivy-rtags
  :straight t
  )
(use-package magit
  :straight t
  )
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
(use-package swiper
  :straight t
  )
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
(use-package vertico
  :straight t
  :init
  (vertico-mode)
)

;; Personal configs and functions


;; Define color themes switcher funs


;; Local Variables:
;; mode: lisp-interaction
;; eval: (hs-minor-mode t)
;; End:
