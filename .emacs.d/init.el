(eval-and-compile
  (setq straight-use-package-by-default t)
  (setq straight-cache-autoloads t)
  (setq straight-vc-git-default-clone-depth 1)
  (setq vc-follow-symlinks t)
  (setq straight-check-for-modifications '(check-on-save find-when-checking))
  (setq straight-recipes-emacsmirror-use-mirror nil)

  (defvar bootstrap-version)
  (setq straight-repository-branch "develop") ; TEMPORARY issue with straight, see https://jeffkreeftmeijer.com/emacs-straight-use-package/
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

  ;; Install use-package
  (straight-use-package '(use-package :type built-in))
  (require 'bind-key))

;;
;; Plugins
;;

(use-package exec-path-from-shell
  :commands exec-path-from-shell-copy-env
  :init
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package centaur-tabs
  :demand t
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-enable-key-bindings t)
  :config
  ;; (centaur-tabs-change-fonts 'default 120)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package doom-modeline
  :functions (doom-modeline-mode)
  :commands (doom-modeline-mode)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32))

(use-package doom-themes
  :demand t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)

  :config
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
  ;; (load-theme 'doom-tomorrow-day t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package dashboard
  :demand t
  :after all-the-icons
  :init
  (setq dashboard-items '((projects . 5)
			                    (recents . 5)))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'project-el)

  (defun k-l/dashboard-init-info ()
    (let ((package-count 0))
      (when (bound-and-true-p package-alist)
        (setq package-count (length package-activated-list)))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
      (when (fboundp 'elpaca--queued)
        (setq package-count (length (elpaca--queued))))
      (format "%d packages loaded in %s with %d garbaged collections."
              package-count
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done)))

  (setq dashboard-init-info 'k-l/dashboard-init-info)

  :config
  (dashboard-setup-startup-hook))

(use-package evil
  :demand t

  :init
  (setq evil-want-keybinding nil)

  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (evil-set-initial-state 'dashboard-mode 'normal)

  :bind
  (:map evil-normal-state-map
        ("g n" . centaur-tabs-forward)
        ("g p" . centaur-tabs-backward)
	      ("g c" . centaur-tabs--create-new-empty-buffer)
	      ("g W" . centaur-tabs-switch-group)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package evil-easymotion
  :defer 2
  :config
  (evilem-default-keybindings "SPC"))

(use-package origami
  :hook
  (prog-mode . origami-mode)
  (yaml-mode . origami-mode))

(use-package vertico
  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :config
  (vertico-mode))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after marginalia
  :config
  (all-the-icons-completion-mode)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
	       ("C-x C-b" . consult-buffer)                ;; orig. switch-to-buffer
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
	       ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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
         ("C-c f" . consult-recent-file)

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

(use-package consult-flycheck)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :bind
  (("C-S-a" . embark-act)
   ("C-S-d" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vterm
  :defer 1
  :bind
  ("C-u" . vterm--self-insert)
  :hook
  (vterm-mode . evil-emacs-state))

(use-package multi-vterm
  :after vterm
  :bind
  ("C-`" . multi-vterm-project)
  ("C-~" . multi-vterm-dedicated-toggle))

(use-package which-key
  :defer 2
  :config
  (which-key-mode))

(use-package transpose-frame
  :bind
  ("M-S-SPC" . transpose-frame))

(use-package company
  :defer t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :hook
  (emacs-startup . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :defer t
  :hook (company-mode . company-box-mode))

(use-package magit
  :defer t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dirvish
  :defer t

  :init
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
  (dirvish-override-dired-mode)
  (evil-make-overriding-map dirvish-mode-map 'normal) ;; to fix https://github.com/alexluigit/dirvish/issues/188

  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)

  :bind*
  ("M-b" . dirvish-side)

  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)))

(use-package eldoc-box
  :defer 2)

(use-package copilot
  :defer t
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))

  :config
  (setq copilot-idle-delay 0.1)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("rust-ts" . "rust"))

  :hook
  (prog-mode . copilot-mode))

(use-package gptel
  :defer t
  :init
  (setq gptel-model "gpt-4"))

(use-package gptel-extensions
  :defer t
  :after gptel
  :straight (:host github :repo "kamushadenes/gptel-extensions.el" :files ("gptel-extensions.el")))

(use-package electric
  :defer 2
  :straight (:type built-in)
  :init
  (setq electric-pair-preserve-balance nil)
  :config
  (electric-pair-mode +1))

(use-package whitespace
  :straight (:type built-in)
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))

(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode))

(use-package git-gutter
  :defer 2
  :config
  (global-git-gutter-mode t))

(use-package ligature
  :config
  (ligature-set-ligatures '(prog-mode)
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (set-face-attribute 'default nil :family "Iosevka" :height 130)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  (setq-default line-spacing 1)
  :hook
  (prog-mode . ligature-mode))

;;
;; Language modes
;;

(add-hook 'prog-mode-hook 'global-prettify-symbols-mode)

;; Only run delete-trailing-whitespace when in prog-mode and save file
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)))

(add-to-list 'auto-mode-alist '(".tsx?$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '(".*rc$" . bash-ts-mode))


(use-package treesit-auto
  :defer 2
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hint-enable t)

  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-inlay-hints-mode t)

  :hook
  (terraform-mode . lsp)
  (rust-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (go-ts-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration) ;; which-key integration
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)

  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-hover t))

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

(use-package rustic
  :defer t
  :custom
  (rustic-format-on-save t)
  (rustic-format-trigger 'on-save))

(use-package php-mode
  :defer t)

(use-package web-mode
  :defer t
  :mode
  (".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   ))

(use-package k8s-mode
  :mode
  ("deployment.*.yaml$"
   "configmap.*.yaml$"
   "secret.*.yaml$"
   "service.*.yaml$"
   "ingress.*.yaml$"
   "hpa.*.yaml$"
   "_helpers.tpl"))

(use-package go-mode
  :defer t)

(use-package cider
  :defer t)

;;
;; Org mode
;;

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package valign
  :hook
  (org-mode . valign-mode))

(use-package org-modern
  :init
  (setq org-modern-table nil)
  (setq org-modern-block-fringe 1)
  :hook
  (org-mode . org-modern-mode))

(use-package org-preview-html
  :defer t
  :commands org-preview-html-mode
  :init
  (setq org-preview-html-viewer 'xwidget))



;;
;; Custom functions
;;
(defun k-l/open-user-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun k-l/alpha-toggle ()
  "toggle transparency"
  (interactive)
  (let* ((current-transparency (frame-parameter nil 'alpha))
         (new-transparency (if (equal current-transparency '(100 . 100))
                               '(90 . 90)
                             '(100 . 100))))
    (set-frame-parameter nil 'alpha new-transparency)
    (add-to-list 'default-frame-alist `(alpha . ,new-transparency))))

(bind-key* "M-[" 'previous-buffer)
(bind-key* "M-]" 'next-buffer)
(bind-key* "M-k ," 'k-l/open-user-config)
(bind-key* "M-k t" 'k-l/alpha-toggle)
