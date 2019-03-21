(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

(require 'cl) ; to compensate for a bug in prog-mode in Emacs 24.5

;; set local package locations
(unless (file-directory-p "~/.emacs.d/lisp")
  (make-directory "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Appearance Settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(use-package darcula-theme
  :ensure t)
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))
(setq inhibit-startup-screen t)

(defun my-after-init-hook ()
  (if (file-directory-p "~/org/org-docs")
      (find-file "~/org/org-docs/index.org")
    (find-file "~/org/work.org"))
  (show-all))
(add-hook 'after-init-hook 'my-after-init-hook)

(use-package delight
  :ensure t
  :config
  (delight '((company-box-mode nil "company-box")
             (company-mode nil "company")
             (yas-minor-mode nil "yasnippet")
             (smartparens-mode nil "smartparens")
             (helm-mode nil "helm")
             (flyspell-mode nil "flyspell")
             (undo-tree-mode nil "undo-tree")
             (highlight-symbol-mode nil "highlight-symbol")
             (flycheck-mode nil "flycheck")
             (lsp-mode nil "lsp-mode")
             (eldoc-mode nil "eldoc")
             (abbrev-mode nil "abbrev"))))

;;; General Configuration
(setq ring-bell-function 'ignore)

(setq default-tab-width 4) ; set tab to appear like 4 spaces

(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

(setq enable-local-eval 't)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t) ; use versioned backups  

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(use-package evil
  :ensure t)
(evil-mode 1)
(setq evil-want-fine-undo t)
(evil-set-initial-state 'term-mode 'emacs)

;; TODO: dired+
(unless (file-exists-p "~/.emacs.d/lisp/dired+.el")
  (progn
    (require 'url)
    (url-copy-file "https://www.emacswiki.org/emacs/download/dired%2b.el"
		   "~/.emacs.d/lisp/dired+.el")))
(load "dired+")
(diredp-toggle-find-file-reuse-dir 1)
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

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
(setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(setenv "TERM" "xterm-256color")
(use-package multi-term
  :ensure t
  :bind ([escape] . term-send-esc)
  :config
  (yas-minor-mode -1)
  (set-face-foreground 'term-color-blue "deep sky blue"))

(defun rename-term (name)
  (interactive "sRename terminal to: ")
  (rename-buffer (concat "*term* " name)))

(defun rename-new-term (name)
  (interactive "sNew terminal name: ")
  (multi-term)
  (rename-term name))

(global-set-key (kbd "s-t") 'multi-term)
(global-set-key (kbd "s-T") 'rename-new-term)

(use-package highlight-symbol
  :ensure t)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(use-package comment-dwim-2
  :ensure t)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(global-set-key (kbd "C-c i") 'clone-indirect-buffer)

(defun narrow-and-rename-region (new-buff-name)
  (interactive "s")
  (let ((beg (region-beginning))
        (end (region-end)))
    (deactivate-mark)
    (save-restriction
      (clone-indirect-buffer new-buff-name t)
      (narrow-to-region beg end))))
(global-set-key (kbd "C-c n") 'narrow-and-rename-region)

(defun narrow-and-rename-defun (new-buff-name)
  (interactive "s")
  (save-restriction
    (clone-indirect-buffer new-buff-name t)
    (narrow-to-defun)))
(global-set-key (kbd "C-c d") 'narrow-and-rename-defun)

(defun narrow-and-rename-page (new-buff-name)
  (interactive "s")
  (save-restriction
    (clone-indirect-buffer new-buff-name t)
    (narrow-to-page)))

; from http://stackoverflow.com/questions/3035337/in-emacs-can-you-evaluate-an-emacs-lisp-expression-and-replace-it-with-the-resul
(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(defun uniquify-all-lines-region (start end)
  "find duplicate lines in region start to end keeping first occurance."
  (interactive "*r")
  (save-excursion
    (let ((lines) (end (copy-marker end)))
      (goto-char start)
      (while (and (< (point) (marker-position end))
                  (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (member line lines)
              (delete-region (point) (progn(forward-line 1) (point)))
            (push line lines)
            (forward-line 1)))))))

(put 'narrow-to-region 'disabled nil)

;;; LaTeX Configuration
(defun my-latex-mode-hook ()
  (visual-line-mode)
  (flyspell-mode)
  (flyspell-buffer))
(add-hook 'latex-mode-hook 'my-latex-mode-hook)

;;; Org Configuration
(setq org-log-done 'time)
(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq org-plantuml-jar-path plantuml-jar-path))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ditaa . t)
   (plantuml . t)
   (latex . t)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-time-clocksum-use-effort-durations t)
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>\n<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>\n\n<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>\n<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>\n<script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.js\"></script>\n<script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>")
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

(use-package adaptive-wrap
  :ensure t)
(defun my/org-mode-hook ()
  (visual-line-mode)
  (org-indent-mode t))
(add-hook 'org-mode-hook 'my/org-mode-hook)

;;; Project Configuration
(use-package skeletor
  :ensure t)
(use-package projectile
  :ensure t
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-directories
        (append '(".cquery_cached_index" ".ccls-cache")
        projectile-globally-ignored-directories)))
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(use-package helm-projectile
  :ensure t)
(helm-projectile-on)
(use-package helm-ag
  :ensure t)

(skeletor-define-template "basic-cpp"
  :title "Basic C++ Project"
  :no-license? t
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "mkdir -p cmake/Modules && mkdir build && cd build && cmake .. && cd .. && ln -sr build/compile_commands.json .")))

(defun my/compile ()
  (interactive)
  (if (string= "-" (projectile-project-name))
      (call-interactively 'compile)
    (call-interactively 'projectile-compile-project))
  )
(global-set-key (kbd "<f5>") 'my/compile)


;; send notifications for compilation success and failure
(defun my/compilation-finish-function (status code msg)
  (if (and (eq status 'exit) (zerop code))
           (notifications-notify
            :title "Compilation success"
            :body msg
            :app-icon "~/.emacs.d/resources/icons/checkmark.png")
      (notifications-notify
       :title "Compilation failure"
       :body msg
       :app-icon "~/.emacs.d/resources/icons/cross.png"))
  (cons msg code))
(setq compilation-exit-message-function 'my/compilation-finish-function)

(defun my/run ()
  (interactive)
  (unless (string= "-" (projectile-project-name))
      (call-interactively 'projectile-run-project)))
(global-set-key (kbd "<f6>") 'my/run)

(use-package magit
  :ensure t)

(use-package cmake-mode
  :ensure t)

;;; ERC Configuration
(require 'erc)
(add-to-list 'erc-modules 'notifications)
(erc-update-modules)

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
(setq yas-expand-only-for-last-commands '(self-insert-command))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))
(use-package company-quickhelp
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

; TODO: this section should be removed once all machines are running emacs > 25!!
(if (> emacs-major-version 25)
    (use-package company-box
      :ensure t
      :hook (company-mode . company-box-mode)))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(setq-default indent-tabs-mode nil)

;;; Compilation Mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "deep sky blue" "magenta3" "cyan3" "gray80"])
(setq ansi-color-map (ansi-color-make-color-map))

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
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  ;; (hl-todo-mode)
  ; A bug causes this to not work correctly
  ; (set (make-local-variable 'comment-auto-fill-only-comments) t)
  ; (auto-fill-mode t)
  )
(add-hook 'c-mode-common-hook 'my-cc-mode-options)

;;; C++ Mode

(defun my-compile-ccls-server ()
  (make-directory "~/.emacs.d/.lsp" t)
  (if (file-directory-p "~/.emacs.d/.lsp/ccls")
      (shell-command "cd ~/.emacs.d/.lsp/ccls && git pull origin master")
    (shell-command "cd ~/.emacs.d/.lsp && git clone https://github.com/MaskRay/ccls --depth=1"))
  (async-shell-command "cd ~/.emacs.d/.lsp/ccls && git submodule update --init && mkdir -p build && cd build && cmake .. && make -j10"))

(unless (file-exists-p "~/.emacs.d/.lsp/ccls/build/ccls")
  (my-compile-ccls-server))

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "~/.emacs.d/.lsp/ccls/build/ccls"))

(defun my-cpp-link-compile-commands ()
  (unless (file-exists-p (concat projectile-cached-project-root "compile-commands.json"))
    (shell-command (concat "cd " projectile-cached-project-root " && ln -s " projectile-cached-project-root "build/compile_commands.json " projectile-cached-project-root "compile_commands.json"))))

(use-package company-lsp
  :ensure t)
(push 'company-lsp company-backends)

(use-package lsp-ui
  :ensure t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(add-hook 'c++-mode-hook 'flycheck-mode)

(add-hook 'c++-mode-hook 'lsp-ccls-enable)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (when projectile-project-name (irony-cdb-json-add-compile-commands-path projectile-project-root (concat projectile-project-root "/build/compile-commands.json"))))

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+class\\s-+")
          (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*")))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

;;; Go Mode
(use-package go-mode
  :ensure t)

(use-package company-go
  :init
  (setenv "GOPATH" (expand-file-name (concat "~/.emacs.d" "/gopkgs")))
  (unless (executable-find "gocode") (shell-command "go get -u github.com/nsf/gocode"))
  (setq company-go-gocode-command (expand-file-name "~/.emacs.d/gopkgs/bin/gocode"))
  :ensure t)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

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

;;; Python Mode
(use-package company-jedi
  :ensure t)
(defun my-python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;;; XML
(setq nxml-slash-auto-complete-flag t)

;;; Web Technologies
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist '(("hugo" . ".*hugo.*html\\'")))
(setq js-indent-level 2)
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))
(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

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

;;; ROS Options
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))

;;; Media and Entertainment
(defun spotify-toggle-play-pause ()
  (interactive)
  (dbus-call-method-asynchronously
   :session "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player" "PlayPause" 'nil))

(defun spotify-pause()
  (interactive)
  (dbus-call-method-asynchronously
   :session "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player" "Pause" 'nil))

(defun spotify-play()
  (interactive)
  (dbus-call-method-asynchronously
   :session "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player" "Play" 'nil))

(defun spotify-next()
  (interactive)
  (dbus-call-method-asynchronously
   :session "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player" "Next" 'nil))

(defun spotify-previous()
  (interactive)
  (dbus-call-method-asynchronously
   :session "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player" "Previous" 'nil))

(global-set-key (kbd "C-c <home>") 'spotify-toggle-play-pause)
(global-set-key (kbd "<XF86AudioPlay>") 'spotify-toggle-play-pause)
(global-set-key (kbd "C-c <prior>") 'spotify-next)
(global-set-key (kbd "<XF86AudioNext>") 'spotify-next)
(global-set-key (kbd "C-c <insert>") 'spotify-previous)
(global-set-key (kbd "<XF86AudioPrev>") 'spotify-previous)

(use-package pdf-tools
  :ensure t)

(when (not (package-installed-p 'pdf-tools))
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'pdf-tools)
  (pdf-tools-install))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;; Start server
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))


(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p "~/.emacs.d/custom.el")
    (load-file custom-file))
