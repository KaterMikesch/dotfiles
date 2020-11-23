;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                   (not (gnutls-available-p))))
      (proto (if no-ssl "http" "https")))
 (when no-ssl
   (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
 ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
 (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
 ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
 (when (< emacs-major-version 24)
   ;; For important compatibility libraries like cl-lib
   (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters toml-mode adoc-mode dylan-mode neotree haskell-mode ergoemacs-mode json-mode markdown-mode use-package cljr-helm company magit kibit-helper helm-projectile projectile helm-clojuredocs helm-cider-history aggressive-indent helm clojure-mode-extra-font-locking clj-refactor cider)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
 (package-install 'use-package))

;; Clojure dev
(use-package cider)
(use-package company)
(use-package rainbow-delimiters)
(use-package clj-refactor)
(use-package paredit)
(use-package aggressive-indent)
(use-package clojure-mode)
(use-package cljr-helm)
(use-package clojure-mode-extra-font-locking)

(unless (package-installed-p 'projectile)
 (package-install 'projectile))

(unless (package-installed-p 'helm-projectile)
 (package-install 'helm-projectile))

(setq projectile-project-search-path '("~/Projects/"))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

(require 'clojure-mode-extra-font-locking)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
   (clj-refactor-mode 1)
   (yas-minor-mode 1) ; for adding require/use/import statements
   ;; This choice of keybinding leaves cider-macroexpand-1 unbound
   (cljr-add-keybindings-with-prefix "C-c C-m"))

(setq cljr-warn-on-eval nil)

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)


(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)


(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)

(setq cider-eldoc-display-context-dependent-info t)

;; Mac support (German)
(global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
(global-set-key "\M-5" '(lambda () (interactive) (paredit-open-square)))
(global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
(global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
(global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
(global-set-key "\M-8" '(lambda () (interactive) (paredit-open-curly)))
(global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
(global-set-key "\M-n" '(lambda () (interactive) (insert "~")))

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
(require 'cljr-helm)
(global-set-key (kbd "C-c r") #'cljr-helm)
(helm-mode 1)

;; toolbar
(tool-bar-mode -1)

;; path
(add-to-list 'exec-path "/usr/local/bin")
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq exec-path (append exec-path '("/usr/local/bin")))

;; delete selection
(delete-selection-mode 1)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; ido
(progn
 ;; make buffer switch command do suggestions, also for find-file command
 (require 'ido)
 (ido-mode 1)

 ;; show choices vertically
 (if (version< emacs-version "25")
     (progn
       (make-local-variable 'ido-separator)
       (setq ido-separator "\n"))
   (progn
     (make-local-variable 'ido-decorations)
     (setf (nth 2 ido-decorations) "\n")))

 ;; show any name that has the chars you typed
 (setq ido-enable-flex-matching t)
 ;; use current pane for newly opened file
 (setq ido-default-file-method 'selected-window)
 ;; use current pane for newly switched buffer
 (setq ido-default-buffer-method 'selected-window)
 ;; stop ido from suggesting when naming new file
 (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

;; winner mode
(winner-mode 1)

;; persistent frame locations/sizes
;;(desktop-save-mode 1)

(setq visible-bell 1)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(xterm-mouse-mode 1)

(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          ("->" . 10230)    ; ⟶
          (">=" . 10878)    ; ⩾
          ("<=" . 10877)    ; ⩽
          ("not=" . 8800)    ; ≠
	  )))

(add-hook 'clojure-mode-hook 'my-add-pretty-lambda)
(add-hook 'haskell-mode-hook 'my-add-pretty-lambda)
(add-hook 'shen-mode-hook 'my-add-pretty-lambda)
(add-hook 'tex-mode-hook 'my-add-pretty-lambda)

(global-prettify-symbols-mode 1)

(set-face-attribute 'default nil :height 140)

;; neotree
 (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

 (global-set-key [f8] 'neotree-project-dir)

;; adoc mode
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
