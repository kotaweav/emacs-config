(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

(require 'cl) ; to compensate for a bug in prog-mode in Emacs 24.5

;; Appearance Settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(use-package darcula-theme
  :ensure t)
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))
(setq inhibit-startup-screen t)

(defun my-after-init-hook ()
  (find-file "~/org/work.org")
  (show-all))
(add-hook 'after-init-hook 'my-after-init-hook)

;;; General Configuration
(setq ring-bell-function 'ignore)

(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

(setq enable-local-eval 't)

(use-package evil
  :ensure t)
(evil-mode 1)

(use-package dired+
  :ensure t)
(diredp-toggle-find-file-reuse-dir 1)
(put 'dired-find-alternate-file 'disabled nil)

(use-package helm
  :ensure t)
(require 'helm-config) ; helm-config is part of the helm package
(helm-mode 1)
(setq helm-display-header-line nil)
(set-face-attribute 'helm-source-header nil :height 0.1)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(use-package multi-term
  :ensure t)

(use-package highlight-symbol
  :ensure t)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;; Org Configuration
(setq org-log-done 'time)

;;; Project Configuration
(use-package skeletor
  :ensure t)
(use-package projectile
  :ensure t)
(projectile-global-mode)
(use-package helm-projectile
  :ensure t)
(helm-projectile-on)

(skeletor-define-template "basic-cpp"
  :title "Basic C++ Project"
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "mkdir -p cmake/Modules && mkdir build && cd build && cmake ..")))

(defun my/compile ()
  (interactive)
  (if (string= "-" (projectile-project-name))
      (call-interactively 'compile)
    (call-interactively 'projectile-compile-project))
  )
(global-set-key (kbd "<f5>") 'my/compile)

(use-package magit
  :ensure t)

(use-package cmake-mode
  :ensure t)

;;; Global Programming Modes
(use-package smartparens
  :ensure t)
(require 'smartparens-config)
(sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my/prog-mode ()
  (flyspell-prog-mode)
  (smartparens-mode)
  (show-smartparens-mode))
(add-hook 'prog-mode-hook 'my/prog-mode)

(use-package yasnippet
  :ensure t)
(yas-global-mode 1)

(use-package company
  :ensure t)
(use-package company-quickhelp
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(setq-default indent-tabs-mode nil)

;;; Arduino Mode
(use-package arduino-mode
  :ensure t)
;(use-package company-arduino
;  :ensure t)

;(defun my-company-c-headers-get-system-path ()
;  "Return the system include path for the current buffer."
;  (let ((default '("/usr/include/" "/usr/local/include/")))
;    (company-arduino-append-include-dirs default t)))
;(setq company-c-headers-path-system 'my-company-c-headers-get-system-path)

;(add-hook 'irony-mode-hook 'company-arduino-turn-on)

;(add-hook 'arduino-mode-hook 'irony-mode)

;;; CC Mode
(setq-default c-basic-offset 4)
(setq-default c-doc-comment-style 'javadoc)
(setq-default c-block-comment-prefix "* ")

(setq cc-other-file-alist
      '(("\\.c"   (".h"))
       ("\\.cpp"   (".h"))
       ("\\.h"   (".c"".cpp"))))

; TODO: this should look for the 'src' folder and create respective 'include' directory
(setq ff-search-directories
      '("." "../src" "../include"))

;;; Bind the toggle function to a global key
(global-set-key "\M-t" 'ff-find-other-file)
(defun my-cc-mode-options ()
  (setq fill-column 80)
  (c-set-offset 'innamespace [0])
  ; A bug causes this to not work correctly
  ; (set (make-local-variable 'comment-auto-fill-only-comments) t)
  ; (auto-fill-mode t)
  )
(add-hook 'c-mode-common-hook 'my-cc-mode-options)

;;; C++ Mode

(use-package irony
  :ensure t)
(use-package company-irony
  :ensure t)
(use-package company-irony-c-headers
  :ensure t)
(use-package flycheck-irony
  :ensure t)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(add-hook 'c++-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'flycheck-mode)

;;; Ruby Mode
(use-package rvm
  :ensure t)
(rvm-use-default)

(use-package robe
  :ensure t)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-robe)))

;;; Markdown Mode
(use-package markdown-mode
  :ensure t)
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)
(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                   org-table-last-alignment ""))
     (params2
      (list
       :splice t
       :hline (concat alignment "|")
       :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))
