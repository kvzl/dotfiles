;;; -*- lexical-binding: t; -*-

;; misc
(setq use-package-verbose t)

(setq confirm-kill-processes nil)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
	      dired-listing-switches "-aBhl --group-directories-first"))

(setq confirm-kill-emacs #'yes-or-no-p)

;;
;; Basic appearance settings
;;
(setq inhibit-startup-message t) ;; Disable default startup message
(scroll-bar-mode -1)		 ;; Disable visible scrollbar
(tool-bar-mode -1)		 ;; Disable the toolbar
(set-fringe-mode 4)		 ;; Give some breathing room

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq evil-shift-width 2)

;; Fonts
(setq-default line-spacing 2)
(set-face-attribute 'default nil :font "Fira Code" :height 130)

(let ((frame-transparency '(90 . 90)))
  (set-frame-parameter (selected-frame) 'alpha frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,frame-transparency)))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		            vterm-mode-hook
		            dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;
;; Plugins
;;

;; Use straight.el as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default 1)
(straight-use-package 'use-package)

;; Manage packages with use-package
(eval-when-compile
  (require 'use-package))

(use-package vterm
  :defer 1
  :hook
  (vterm-mode-hook . evil-emacs-state))

(use-package multi-vterm
  :defer 1
  :config
  (add-hook 'vterm-mode-hook
	          (lambda ()
	            (setq-local evil-insert-state-cursor 'box)
	            (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)

  :bind
  ("C-`" . multi-vterm-project))

(use-package dashboard
  :after all-the-icons
  :config
  (setq dashboard-items '((projects . 5)
			                    (recents . 5)))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'project-el)
  (dashboard-setup-startup-hook))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package consult
  :bind (
	       ("C-c M-x" . consult-mode-command)
	       ("C-c h" . consult-history)
	       ("C-c k" . consult-kmacro)
	       ("C-c m" . consult-man)
	       ("C-c i" . consult-info)
	       ([remap Info-search] . consult-info)

	       ;; C-x bindings in `ctl-x-map'
	       ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	       ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	       ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	       ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	       ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	       ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

	       ;; Custom M-# bindings for fast register access
	       ("M-#" . consult-register-load)
	       ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	       ("C-M-#" . consult-register)

	       ;; Other custom bindings
	       ("M-y" . consult-yank-pop)                ;; orig. yank-pop

	       ;; M-g bindings in `goto-map'
	       ("M-g e" . consult-compile-error)
	       ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	       ("M-g g" . consult-goto-line)             ;; orig. goto-line
	       ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	       ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	       ("M-g m" . consult-mark)
	       ("M-g k" . consult-global-mark)
	       ("M-g i" . consult-imenu)
	       ("M-g I" . consult-imenu-multi)

	       ;; M-s bindings in `search-map'
	       ("M-s d" . consult-find)
	       ("M-s D" . consult-locate)
	       ("M-s g" . consult-grep)
	       ("M-s G" . consult-git-grep)
	       ("M-s r" . consult-ripgrep)
	       ("M-s l" . consult-line)
	       ("M-s L" . consult-line-multi)
	       ("M-s k" . consult-keep-lines)
	       ("M-s u" . consult-focus-lines)

	       ;; Isearch integration
	       ("M-s e" . consult-isearch-history)
	       :map isearch-mode-map
	       ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	       ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	       ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	       ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	       )

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :defer 2
  :after marginalia
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


(use-package which-key
  :defer 2
  :config
  (which-key-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; ===> Dark themes
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-opera t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-homage-black t)
  ;; (load-theme 'doom-plain-dark t)
  (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-ayu-dark t)
  ;; (load-theme 'doom-ayu-mirage t)

  ;; ===> Light themes
  ;; (load-theme 'doom-flatwhite t)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-homage-white t)
  ;; (load-theme 'doom-ayu-light t)
  ;; (load-theme 'doom-plain t)


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package edwina
  :defer 1
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  :init
  (edwina-mode 1))

(use-package transpose-frame
  :defer 3
  :bind
  ("M-S-SPC" . transpose-frame))


(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :defer 2
  :config
  (evil-collection-init))

(use-package evil-surround
  :defer 2
  :config
  (global-evil-surround-mode 1))


(use-package company
  :defer 2
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :defer 2
  :hook (company-mode . company-box-mode))

(use-package magit
  :defer t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dirvish
  :defer t

  :config
  (setq dirvish-attributes
	      '(vc-state
	        subtree-state
	        all-the-icons
	        collapse
	        git-msg
	        file-time
	        file-size))

  (setq dirvish-header-line-format
	      '(:left
	        (path)
	        :right
	        (free-space)))

  (setq dirvish-mode-line-format
	      '(:left
	        (sort file-time " " file-size symlink)
	        :right
	        (omit yank index)))

  :init
  (dirvish-override-dired-mode))

(use-package eldoc-box
  :defer 3)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))

  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("rust-ts" . "rust"))

  :hook
  (prog-mode . copilot-mode))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "PATH")))

(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-enable-key-bindings t)
  :config
  ;; (centaur-tabs-change-fonts 'default 120)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :bind
  (:map evil-normal-state-map
        ("g n" . centaur-tabs-forward)
        ("g p" . centaur-tabs-backward)
	      ("g c" . centaur-tabs--create-new-empty-buffer)
	      ("g W" . centaur-tabs-switch-group)))

;;
;; Language modes
;;

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eglot
  :defer t
  :straight (:type built-in)

  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))

  :hook
  (terraform-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure))

(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package yaml-mode
  :defer t)

(use-package dotenv-mode
  :defer t)

(use-package fsharp-mode
  :defer t)

(use-package terraform-mode
  :defer t
  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save t))

(use-package gitlab-ci-mode
  :defer t)

(use-package rust-mode
  :defer t
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . prettify-symbols-mode))

;; 
;; Global key bindings
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(defun open-user-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(bind-key "M-," 'open-user-config override-global-map)

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
