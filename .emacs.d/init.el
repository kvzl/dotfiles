;; emacs settings
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode)
;; (global-display-line-numbers-mode)
(add-to-list 'default-frame-alist
	     '(font . "Fira Code-12"))
(setq-default line-spacing 6)
;; (toggle-frame-fullscreen)


;; straight.el
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


;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; mac os
(cond ((string-equal system-type "darwin")
       (progn
         ;; modify option and command key
         (setq mac-option-modifier 'meta)
	 (setq mac-option-modifier 'meta)
	 (if (fboundp 'mac-auto-operator-composition-mode)
	     (mac-auto-operator-composition-mode))

	 ;; batter copy and paste support for mac os x
	 (defun copy-from-osx ()
           (shell-command-to-string "pbpaste"))

         (defun paste-to-osx (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
               (process-send-string proc text)
               (process-send-eof proc))))
         (setq interprogram-cut-function 'paste-to-osx)
         (setq interprogram-paste-function 'copy-from-osx)

         ;;(use-package exec-path-from-shell)
         ;;(when (memq window-system '(mac ns x))
         ;; (exec-path-from-shell-theme))
	 )))


;; initialize
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;; all-the-icons
(use-package all-the-icons
  :ensure t)


;; dashboard
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode)
  (use-package dashboard
    :ensure t
    :config
    (setq initial-buffer-choice (lambda ()
				  (get-buffer "*dashboard*")))
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (dashboard-setup-startup-hook))
  )


;; neotree
(use-package neotree
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (setq neo-window-fixed-size nil)
    ;; (setq-default neo-show-hidden-files nil)
    (global-set-key [f2] 'neotree-toggle)
    (global-set-key [f8] 'neotree-dir)))


;; evil
(use-package evil
  :ensure t
  :init 
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-collection
  :after evil
  :ensure t
  :config 
  (evil-collection-init))


;; projectile
(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))


;; vue
(use-package vue-mode)
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))


;; magit
(use-package magit
  :ensure t
  :bind (("\C-x g" . magit-status))
  )


;; autopair
(use-package autopair
  :config (autopair-global-mode) )


;; company
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))


;; linum
(use-package linum
  :init
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d  ")
    (set-face-background 'linum nil)))


;; git-gutter-plus
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish (git-gutter+-mode . "gg"))


;; elixir
(use-package elixir-mode)
