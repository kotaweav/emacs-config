;; emacs built with:
;; ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3 --without-compress-install --with-native-compilation --with-json --with-tree-sitter --with-imagemagick

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(let ((emacs-git "~/.emacs.d/git"))
  (mapc (lambda (x)
          (add-to-list 'load-path (expand-file-name x emacs-git)))
        (delete ".." (directory-files emacs-git))))

(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
(use-package quelpa-use-package
  :ensure t)
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)
(setq treesit-font-lock-level 4)

(require 'cl) ; to compensate for a bug in prog-mode in Emacs 24.5

;; set local package locations
(unless (file-directory-p "~/.emacs.d/lisp")
  (make-directory "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/lisp")

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

;;; Appearance Settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(use-package obsidian-theme
  :ensure t
  :config
  (enable-theme 'obsidian)
  (set-face-attribute 'mode-line-active nil
                      :foreground "medium spring green"
                      :background "#132024"))

(use-package nerd-icons
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t))

(global-hl-line-mode 1)
;; underline the current line
(set-face-attribute hl-line-face nil)

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))
(setq inhibit-startup-screen t)
(setq-default show-trailing-whitespace t)

(fringe-mode '(20 . 20))
(defun my-after-init-hook ()
  (if (file-directory-p "~/org/org-docs")
      (find-file "~/org/org-docs/index.org")
    (find-file "~/org/work.org"))
  (show-all))
(add-hook 'after-init-hook 'my-after-init-hook)
(global-unset-key (kbd "C-x C-z")) ;; get rid of automatically suspending frame!

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

(global-unset-key (kbd "C-x C-c"))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package gnu-elpa-keyring-update
  :ensure t)

(setq ring-bell-function 'ignore)

(setq-default tab-width 4) ; set tab to appear like 2 spaces

(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

(setq enable-local-eval 't)

(global-set-key (kbd "<XF86Tools>") 'kill-region)
(global-set-key (kbd "<XF86Launch5>") 'kill-ring-save)
(global-set-key (kbd "<XF86Launch6>") 'yank)
(global-set-key (kbd "C-s-o") 'other-window)
(global-set-key (kbd "C-s-p") (lambda () (interactive) (other-window -1)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package drag-stuff
  :ensure t)
(add-to-list 'drag-stuff-except-modes 'org-mode)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   create-lockfiles nil
   version-control t) ; use versioned backups  

(defun download-file-to-directory (url filename)
  (interactive (list (read-string "URL: ")
                     (read-string "File name: ")))
    (let ((buffer (url-retrieve-synchronously url)))
        (save-excursion
        (set-buffer buffer)
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file filename))
        (kill-buffer buffer)))

;; (use-package dtrt-indent
  ;; :ensure t
  ;; :config
  ;; (dtrt-indent-global-mode))

(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.undo")))

(use-package evil
  :ensure t
  :init
  (setq evil-want-fine-undo t)
  :config
	(evil-mode))
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-undo-system 'undo-tree)

;; (use-package evil
;;   :ensure t)
;; (evil-mode 1)
;; (setq evil-want-fine-undo t)
;; (evil-set-initial-state 'term-mode 'emacs)
;; (evil-set-undo-system 'undo-tree)

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode)
  (eyebrowse-setup-opinionated-keys))

(add-to-list 'default-frame-alist '(close-inner-windows . nil))

(defun my/close-frame-function(frame)
  (if (frame-parameter nil 'close-inner-windows)
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (< (length (get-buffer-window-list buffer nil t)) 2)
              (kill-buffer buffer))))))

(global-set-key (kbd "C-<tab>") 'other-frame)

(add-hook 'delete-frame-functions 'my/close-frame-function)

(defun my/toggle-fixed-window()
  (interactive)
  (cond
   ((and (window-fixed-size-p)
         (window-dedicated-p))
    (setq window-fixed-size nil)
    (set-window-dedicated-p (selected-window) nil)
    (window-preserve-size (selected-window) nil)
    (message "Window is now resizable"))
   ((and (not (window-fixed-size-p))
         (not (window-dedicated-p)))
    (setq window-fixed-size t)
    (set-window-dedicated-p (selected-window) t)
    (window-preserve-size (selected-window) t)
    (message "Window is now fixed"))
   ((window-dedicated-p)
    (setq window-fixed-size t)
    (window-preserve-size (selected-window) t)
    (message "Window is now fixed. Be careful, originally dedicated."))
   (t
    (message "Could not toggle window state"))))

(global-set-key (kbd "C-c t") 'my/toggle-fixed-window)

(defun prompt-save-daemon (frame)
  (when (and (member terminal-frame (visible-frame-list))
             (< (length (visible-frame-list)) 3))
    (save-some-buffers)))

(add-hook 'delete-frame-functions 'prompt-save-daemon)

(defun my/set-eyebrowse-frame-title()
  (interactive)

  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (current-config (assq (eyebrowse--get 'current-slot) window-configs))
       (window-tag (car (last current-config)))
       title
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (first-count (caar window-configs))
       (last-count (caar (last window-configs))))
    (if (= (length window-tag) 0)
        (setq title (concat (number-to-string (car current-config)) "@emacs"))
      (setq title (concat (car (last current-config)) "@emacs")))
    (if (equal curr-count first-count)
        (set-frame-name (concat title " >"))
      (if (equal curr-count last-count)
          (set-frame-name (concat "< " title))
        (set-frame-name (concat "< " title " >")))))
  (force-mode-line-update))

(add-hook 'eyebrowse-post-window-switch-hook 'my/set-eyebrowse-frame-title)

(defun my/move-ws-left()
  (interactive)
  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (first-count (caar window-configs)))
    (if (equal curr-count first-count)
        (eyebrowse-switch-to-window-config (1- first-count))
      (eyebrowse-prev-window-config nil))))

(defun rename-ws(tag)
  (interactive "MWorkspace Name: ")
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)
  (my/set-eyebrowse-frame-title))

(defun my/move-ws-right ()
  (interactive)
  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (last-count (caar (last window-configs))))
    (if (equal curr-count last-count)
        (eyebrowse-switch-to-window-config (1+ last-count))
      (eyebrowse-next-window-config nil))))

(define-key eyebrowse-mode-map (kbd "C-<") 'my/move-ws-left)
(define-key eyebrowse-mode-map (kbd "C->") 'my/move-ws-right)

; stolen from https://emacs.stackexchange.com/questions/31646/how-to-paste-with-indent
(defun yank-with-indent ()
  (interactive)
  (let ((indent
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message indent)
    (yank)
    (save-excursion
      (save-restriction
        (narrow-to-region (mark t) (point))
        (pop-to-mark-command)
        (replace-string "\n" (concat "\n" indent))
        (widen)))))

;; (use-package hideshowvis
  ;; :ensure t)

(defun my/hs-minor-mode-hook ()
  (define-key hs-minor-mode-map (kbd "C-c h h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h s") 'hs-show-block))
;; (add-hook 'hs-minor-mode-hook 'my/hs-minor-mode-hook)
;; (add-hook 'prog-mode-hook #'hs-minor-mode)
;; (add-hook 'prog-mode-hook #'hideshowvis-enable)

(use-package origami
  :ensure t
  :bind (("<XF86Launch7>" . origami-toggle-node)
         ("C-<XF86Launch7>" . origami-open-all-nodes))
  :config
  (global-origami-mode))

(defun my/make-frame-on-monitor ()
  (interactive)
                                        ; run transparent_window.py in the background
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*"
          (cons #'display-buffer-no-window nil))))
       (default-frame-alist (cl-pushnew '(fullscreen . maximized) default-frame-alist)))
    (unwind-protect
        (progn
          (async-shell-command "python3 ~/.emacs.d/scripts/transparent_window.py" nil nil)
          (call-interactively 'make-frame-on-monitor))
      (progn
        (set-process-query-on-exit-flag (get-buffer-process "*Async Shell Command*") nil)
        (kill-buffer "*Async Shell Command*")))))

(global-set-key (kbd "s-f") 'my/make-frame-on-monitor)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)
(setq dired-mouse-drag-files t)
;(set-face-foreground 'dired-directory "aqua") ;

(use-package helm
  :ensure t)
;(require 'helm-config) ; helm-config is part of the helm package
(helm-mode 1)
(setq helm-display-header-line nil)
(add-hook 'after-make-frame-functions (lambda (foo) (set-face-attribute 'helm-source-header nil :height 0.1)))
(set-face-attribute 'helm-source-header nil :height 0.1)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
(setq helm-split-window-in-side-p t)
(setq helm-buffer-skip-remote-checking t)
(setq helm-frame-background-color "#202527")

(setq helm-display-buffer-width 90)

; taken from default helm-core.el
(defun my/helm-display-buffer-in-own-frame (buffer &optional resume)
  "Display Helm buffer BUFFER in a separate frame.

Function suitable for `helm-display-function',
`helm-completion-in-region-display-function' and/or
`helm-show-completion-default-display-function'.

See `helm-display-buffer-height' and `helm-display-buffer-width'
to configure frame size.

Note that this feature is available only with emacs-25+.
Note also it is not working properly in helm nested session with emacs
version < emacs-28."
  (cl-assert (and (fboundp 'window-absolute-pixel-edges)
                  (fboundp 'frame-geometry))
             nil "Helm buffer in own frame is only available starting at emacs-25+")
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((parent (selected-frame))
           (frame-pos (frame-position parent))
           (parent-left (car frame-pos))
           (parent-top (cdr frame-pos))
           (pos (window-absolute-pixel-position))
           (half-screen-size (/ (display-pixel-height x-display-name) 2))
           (frame-info (frame-geometry))
           (prmt-size (length helm--prompt))
           (line-height (frame-char-height))
           tab-bar-mode
           (new-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((width . ,helm-display-buffer-width)
                 (height . ,helm-display-buffer-height)
                 (tool-bar-lines . 0)
                 ;; (left . ,(- (car pos)
                 ;;             (* (frame-char-width)
                 ;;                (if (< (- (point) (point-at-bol)) prmt-size)
                 ;;                    (- (point) (point-at-bol))
                 ;;                  prmt-size))))
                 ;; Try to put frame at the best possible place.
                 ;; Frame should be below point if enough
                 ;; place, otherwise above point and
                 ;; current line should not be hidden
                 ;; by helm frame.
                 ;; (top . ,(if (> (cdr pos) half-screen-size)
                 ;;             ;; Above point
                 ;;             (- (cdr pos)
                 ;;                ;; add 2 lines to make sure there is always a gap
                 ;;                (* (+ helm-display-buffer-height 2) line-height)
                 ;;                ;; account for title bar height too
                 ;;                (cddr (assq 'title-bar-size frame-info)))
                 ;;           ;; Below point
                 ;;           (+ (cdr pos) line-height)))

                 ;; (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
                 ;; (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 6)))
                 ;; (left . ,(+ parent-left (/ (- (* (frame-char-width parent) (frame-width parent)) ,helm-display-buffer-width) 2)))
                 (left . ,(+ parent-left (/ (- (* (frame-char-width parent) (frame-width parent)) (* (frame-char-width parent) helm-display-buffer-width)) 2)))

                 ;; (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))

                 (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 6)))
                 (title . "Helm")
                 (undecorated . ,helm-use-undecorated-frame-option)
                 (background-color . ,(or helm-frame-background-color
                                          (face-attribute 'default :background)))
                 (foreground-color . ,(or helm-frame-foreground-color
                                          (face-attribute 'default :foreground)))
                 (alpha . ,(or helm-frame-alpha 100))
                 (font . ,(assoc-default 'font (frame-parameters)))
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visibility . ,(null helm-display-buffer-reuse-frame))
                 (minibuffer . t))))
           display-buffer-alist)
      ;; Display minibuffer above or below only in initial session,
      ;; not on a session triggered by action, this way if user have
      ;; toggled minibuffer and header-line manually she keeps this
      ;; setting in next action.
      (unless (or helm--executing-helm-action resume)
        ;; Add the hook inconditionally, if
        ;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
        ;; will have anyway no effect so no need to remove the hook.
        (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
        ;; (with-helm-buffer
        ;;   (setq-local helm-echo-input-in-header-line
        ;;               (not (> (cdr pos) half-screen-size))))
        )
      (helm-display-buffer-popup-frame buffer new-frame-alist)
      ;; When frame size have been modified manually by user restore
      ;; it to default value unless resuming or not using
      ;; `helm-display-buffer-reuse-frame'.
      ;; This have to be done AFTER raising the frame otherwise
      ;; minibuffer visibility is lost until next session.
      (unless (or resume (not helm-display-buffer-reuse-frame))
        (set-frame-size helm-popup-frame
                        helm-display-buffer-width
                        helm-display-buffer-height)))
    (helm-log-run-hook "my/helm-display-buffer-in-own-frame" 'helm-window-configuration-hook)))


; taken from https://www.reddit.com/r/emacs/comments/jj269n/display_helm_frames_in_the_center_of_emacs/
(defun my-helm-display-frame-center (buffer &optional resume)
  "Display `helm-buffer' in a separate frame which centered in
parent frame."
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((parent (selected-frame))
           (frame-pos (frame-position parent))
           (parent-left (car frame-pos))
           (parent-top (cdr frame-pos))
           (width (/ (frame-width parent) 2))
           (height (/ (frame-height parent) 3))

           ;; (width (max (/ (frame-width parent) 2) 300))
           ;; (height (max (/ (frame-height parent) 3) 200))
           tab-bar-mode
           (default-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((parent . ,parent)
                 (width . ,width)
                 (height . ,height)
                 (undecorated . ,helm-use-undecorated-frame-option)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (tool-bar-lines . 0)
                 (line-spacing . 0)
                 (desktop-dont-save . t)
                 (no-special-glyphs . t)
                 (inhibit-double-buffering . t)
                 (tool-bar-lines . 0)
                 (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
                 (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 6)))
                 (title . "Helm")
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visibility . ,(null helm-display-buffer-reuse-frame))
                 ;; (internal-border-width . ,(if IS-MAC 1 0))
                )))
           display-buffer-alist)
      ;; (set-face-background 'internal-border (face-foreground 'default))
      ;; (raise-frame)
      (helm-display-buffer-popup-frame buffer default-frame-alist))
      ;; (unless (bound-and-true-p 'helm-popup-frame)
        ;; (setq helm-popup-frame (selected-frame)))
      ;; (select-frame-set-input-focus (selected-frame)))

    ;; (select-frame-set-input-focus helm-popup-frame)
    ;; (select-frame-set-input-focus )
    (helm-log-run-hook "my-helm-display-frame-center" 'helm-window-configuration-hook)))
(setq helm-display-function 'my/helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t)
;; (setq helm-display-function 'my-helm-display-frame-center)

;; (add-hook 'helm-major-mode-hook
;;           (lambda ()
;;             (message "enter helm")))
(use-package helm-lsp
  :ensure t)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(customize-set-variable 'helm-ff-lynx-style-map t)
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(setenv "TERM" "xterm-256color")
(use-package multi-term
  :ensure t
  :bind ([escape] . term-send-esc)
  :config
  (yas-minor-mode -1)
  (set-face-foreground 'term-color-blue "deep sky blue"))

(defun my-compile-libvterm ()
  (make-directory "~/.emacs.d/.vterm" t)
  (if (file-directory-p "~/.emacs.d/.vterm/emacs-libvterm")
      (shell-command "cd ~/.emacs.d/.vterm/emacs-libvterm && git pull origin master")
    (shell-command "cd ~/.emacs.d/.vterm && git clone https://github.com/akermu/emacs-libvterm.git"))
  (async-shell-command "cd ~/.emacs.d/.vterm/emacs-libvterm && git pull origin master && mkdir build -p build && cd build && cmake .. && make -j"))

(unless (file-exists-p "~/.emacs.d/.vterm/emacs-libvterm")
  (my-compile-libvterm))
(add-to-list 'load-path "~/.emacs.d/.vterm/emacs-libvterm")
(require 'vterm)
(add-to-list 'evil-emacs-state-modes 'vterm-mode)
(use-package multi-vterm
         :ensure t)

(defun my/vterm-hook()
  (define-key vterm-mode-map (kbd "<escape>") (lambda ()
                                                (interactive)
                                                (vterm-send-key "<escape>")))
  (setq show-trailing-whitespace nil)
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
  (set-face-foreground 'term-color-blue "deep sky blue"))

(add-hook 'vterm-mode-hook 'my/vterm-hook)

(defun rename-term (name)
  (interactive "sRename terminal to: ")
  (rename-buffer (concat "*term* " name)))

(defun rename-new-term (name)
  (interactive "sNew terminal name: ")
  (multi-vterm)
  (rename-term name))

(defun kill-all-term ()
  (interactive)
  (with-temp-buffer
    (dolist (buff (buffer-list))
      (let ((buff-name (buffer-name buff))
            (buff-mode (buffer-local-value 'major-mode buff)))
        (when (equal (buffer-local-value 'major-mode buff) 'vterm-mode)
          (kill-buffer buff))))))

(global-set-key (kbd "s-t") 'multi-vterm)
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

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;; LaTeX Configuration
(defun my-latex-mode-hook ()
  (visual-line-mode)
  (flyspell-mode)
  (flyspell-buffer))
(add-hook 'latex-mode-hook 'my-latex-mode-hook)

;;; Org Configuration
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(use-package ob-ipython
  :ensure t)
(setq org-log-done 'time)
(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq org-plantuml-jar-path plantuml-jar-path))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)
   (ditaa . t)
   (plantuml . t)
   (ruby . t)
   (shell . t)
   (latex . t)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-image-actual-width 800)
(setq org-export-preserve-breaks t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-time-clocksum-use-effort-durations t)
(setq org-blank-before-new-entry
      '((heading . nil)
        (plain-list-item . nil)))
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>\n<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>\n\n<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>\n<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>\n<script type=\"text/javascript\" src=\"https://unpkg.com/sticky-table-headers\"></script>\n<script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>")
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

(use-package centered-window
  :ensure t)

(defun centered-window-on-switch ()
  (interactive)
  (if (and (not (or (window-in-direction 'left) (window-in-direction 'right))) (bound-and-true-p centered-window-mode-set))
      (unless (bound-and-true-p centered-window-mode)
        (progn
          (centered-window-mode-toggle)))
    (if (bound-and-true-p centered-window-mode)
        (progn
          (centered-window-mode-toggle)))))

(add-hook 'window-configuration-change-hook 'centered-window-on-switch)

(defun toggle-centered-window ()
  (interactive)
  (unless (boundp 'centered-window-mode-set)
    (make-local-variable 'centered-window-mode-set)
    (setq centered-window-mode-set nil))
  (setq centered-window-mode-set (not centered-window-mode-set))
  (centered-window-on-switch))
(global-set-key (kbd "<XF86Launch9>") 'toggle-centered-window)


(defun copy-org-to-clipboard ()
  (interactive)
  (save-excursion
    (let ((buff (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring buff)
        (org-mode)
        (goto-char 0)
        (org-show-all)
        (if (not (org-at-heading-p))
            (progn
              (org-next-visible-heading 1)
              (kill-region (point-min) (point))))
        (while (not (eobp))
          (let ((num-spaces (- (org-current-level) 1)))
            (delete-char num-spaces)
            (insert-char (char-from-name "SPACE") (* num-spaces 2))
            (forward-line 1)
            (while (not (or (eobp) (org-at-heading-p)))
              (progn
                (if (not (eolp))
                    (insert-char (char-from-name "SPACE") (+ (* num-spaces 2) 2)))
                (forward-line 1)))))
        (clipboard-kill-region (point-min) (point-max))))))

(use-package emacsql-sqlite-builtin
  :ensure t)
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/orgroam")
  (org-roam-completion-everywhere t)
  (org-roam-mode-sections (list 'org-roam-backlinks-section
                                'org-roam-reflinks-section))
  :config
  (org-roam-setup))

;; (setq org-agenda-files
;;       `(,(concat (file-name-as-directory org-roam-directory) "20220927084753-daily_habits.org")))
;; (setq org-agenda-prefix-format '(
  ;; (agenda  . " %-12T")))
;; (setq org-agenda-remove-tags t)

(use-package org-roam-ui
  :ensure t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-open-on-start nil
        org-roam-ui-update-on-save t)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org" "\n#+title: ${title}\n#+author: %n\n#+lastmod: [%<%Y-%m-%d %a %H:%M>]\n#+categories[]:\n#+draft: true\n#+ROAM_TAGS:\n#+PROPERTY: header-args :dir \"%<%Y%m%d%H%M%S>-${slug}\"\n#+hugo_bundle: %<%Y%m%d%H%M%S>-${slug}\n#+export_file_name: _index.md\n")
           :unnarrowed t))))

(defun xwidget-webkit-get-url-buffer (url)
  "Returns xwidget buffer that points to URL, nil if none."
  (interactive)
  (when-let* ((r (lambda (x)
                   (when x (replace-regexp-in-string "http\\(s\\|\\)://" "" x))))
              (fnd (seq-find
                    (lambda (x)
                      (string= (concat (funcall r url) "/")
                               (funcall r (xwidget-webkit-uri x))))
                    xwidget-list)))
    (xwidget-buffer fnd)))

(defun xwidget-webkit-url-get-create (url &optional buffer-name)
  "Opens existing xwidget buffer, if it exists for the given URL,
or creates new session. Optionally, BUFFER-NAME can be set"
  (interactive (list (or (thing-at-point 'url)
                         (car (browse-url-interactive-arg "xwidget url: ")))))
  (require 'xwidget)
  (or (xwidget-webkit-get-url-buffer url)
      (progn (xwidget-webkit-browse-url url :new-session)
             (let ((buf xwidget-webkit-last-session-buffer))
               (run-with-timer
                1 nil
                (lambda (buf buffer-name)
                  (with-current-buffer buf
                    (rename-buffer (or buffer-name (concat "*xwidget " url "*")))))
                buf buffer-name)
               buf))))

 (defun org-roam-toggle-ui-xwidget ()
   (interactive)
   (let* ((url (concat "http://localhost:" (number-to-string org-roam-ui-port)))
          (buf (or (xwidget-webkit-get-url-buffer
                    (concat "localhost:" (number-to-string org-roam-ui-port)))
                   (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
     (if-let ((win (get-buffer-window buf)))
         (delete-window win)
       (switch-to-buffer-other-window buf))))

(add-hook 'org-roam-capture-new-node-hook
          (lambda ()
            (message "Created node %s" (buffer-file-name))
            (make-directory (file-name-sans-extension (buffer-file-name)))))

(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "[%04Y-%02m-%02d %a %H:%M]")
(add-hook 'before-save-hook 'time-stamp nil)

(defun my/org-roam-work-only (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "work" tags)))

(defun my/org-roam-personal-only (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "personal" tags)))

(defun my/org-roam-web-only (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "web" tags)))

(defun org-get-all-website-pages ()
  (org-roam-db-query
   [:select file :from nodes :inner :join tags :on (= nodes:id tags:node_id) :where (= tags:tag "web")]))

(setq org-website-path "/home/kota/kota_dev_ws/personal_blog/personal_blog/content/posts")
(defun org-generate-website ()
  (interactive)
  (dolist (source-file-arr (org-get-all-website-pages))
    (let* ((source-file (car source-file-arr))
           (target-file
            (concat (file-name-as-directory org-website-path)
                    (car (last (file-name-split source-file))))))
      (if (file-exists-p target-file)
          (delete-file target-file))
      (copy-file source-file target-file))))

(setq org-hugo-base-dir "/home/kota/kota_dev_ws/personal_blog/personal_blog")
(setq org-hugo-default-section-directory "posts")

(defun org-generate-hugo ()
  (interactive)
  (dolist (source-file-arr (org-get-all-website-pages))
    (let ((source-file (car source-file-arr)))
      (message "exporting %s" source-file)
      (org-hugo--export-file-to-md source-file))))

(use-package adaptive-wrap
  :ensure t)

(defun split-horizontal-center-window ()
  (interactive)
  (if (bound-and-true-p centered-window-mode)
      (progn
        (centered-window-mode-toggle)
        (split-window-horizontally)
        (centered-window-mode-toggle))
    (split-window-horizontally)))
(global-set-key (kbd "C-x 3") 'split-horizontal-center-window)

(use-package org-download
  :ensure t)
(defun my/org-mode-hook ()
  (visual-line-mode)
  (toggle-centered-window)
  (org-indent-mode t)
  (org-download-enable)
  (setq org-confirm-babel-evaluate nil)
  (define-key evil-insert-state-map (kbd "S-<left>") nil)
  (define-key evil-insert-state-map (kbd "S-<right>") nil)
  (local-set-key [?\s-e] 'org-latex-export-to-pdf))
(add-hook 'org-mode-hook 'my/org-mode-hook)

(require 'ox-latex)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-classes
             '("hitec"
             "\\documentclass{hitec}
               \\usepackage{graphicx}
               \\usepackage{graphicx}
               \\usepackage[hidelinks]{hyperref}
               \\usepackage{parskip}
               \\usepackage{pstricks}
               \\usepackage{textcomp}
               \\usepackage[tikz]{bclogo}
               \\usepackage{listings}
               \\usepackage{fancyvrb}
               \\usepackage{xcolor}
               \\definecolor{LightGray}{gray}{0.9}
               \\usepackage[bgcolor=LightGray]{minted}
               \\usemintedstyle{monokai}
               \\presetkeys{bclogo}{ombre=true,epBord=3,couleur = blue!15!white,couleurBord = red,arrondi = 0.2,logo=\bctrombone}{}
               \\usetikzlibrary{patterns}
               \\company{Skylla Technologies}
               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package ox-hugo
  :ensure t
  :after ox)

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
(global-set-key (kbd "C-s-S") 'helm-ag-project-root)

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
  :ensure t
  :bind ("s-g" . magit-status))
;; (use-package forge
  ;; :after magit)
(global-set-key (kbd "s-g") 'magit-status)
(remove-hook 'server-switch-hook 'magit-commit-diff)
(use-package git-link
  :ensure t)
(setq git-link-open-in-browser t)

(use-package cmake-mode
  :ensure t)

;;; ERC Configuration
(require 'erc)
(add-to-list 'erc-modules 'notifications)
(erc-update-modules)

;;; Global Programming Modes

(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

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
(global-set-key (kbd "M-o") 'yas-expand)

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(setq-default indent-tabs-mode nil)

;;; Completion

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-quickhelp
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

(if (> emacs-major-version 25)
    (use-package company-box
      :ensure t
      :hook (company-mode . company-box-mode)))

;; (use-package company-lsp
  ;; :ensure t)
;; (push 'company-lsp company-backends)

;; (use-package straight
  ;; :ensure t)
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)
(add-hook 'prog-mode-hook 'copilot-mode)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

; use this command to enable all:
; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package lsp-ui
  :ensure t
  :bind ("s-." . lsp-ui-peek-find-references))
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

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
(setq-default tab-width 4)
(setq-default c-doc-comment-style 'javadoc)
(setq-default c-block-comment-prefix "* ")

(setq-default c-ts-mode-indent-offset 4)

(setq ff-search-directories
      '("." "../src" "../include"))

;;; Bind the toggle function to a global key
(global-set-key "\M-t" 'ff-find-other-file)
(defun my-cc-mode-options ()
  (setq fill-column 80)
  (c-set-offset 'innamespace [0])
  (c-set-offset 'inlambda 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  ;; (hl-todo-mode)
  ; A bug causes this to not work correctly
  ; (set (make-local-variable 'comment-auto-fill-only-comments) t)
  ; (auto-fill-mode t)
  )
(add-hook 'c-mode-common-hook 'my-cc-mode-options)
(add-hook 'c-ts-base-mode-hook 'my-cc-mode-options)

;;; C++ Mode

;; (defun my-compile-ccls-server ()
;;   (make-directory "~/.emacs.d/.lsp" t)
;;   (if (file-directory-p "~/.emacs.d/.lsp/ccls")
;;       (shell-command "cd ~/.emacs.d/.lsp/ccls && git pull origin master")
;;     (shell-command "cd ~/.emacs.d/.lsp && git clone https://github.com/MaskRay/ccls --depth=1"))
;;   (async-shell-command "cd ~/.emacs.d/.lsp/ccls && git submodule update --init && mkdir -p build && cd build && cmake .. && make -j"))

;; (unless (file-exists-p "~/.emacs.d/.lsp/ccls/build/ccls")
;;   (my-compile-ccls-server))

;; (use-package ccls
;;   :ensure t
;;   :init
;;   (setenv "CPLUS_INCLUDE_PATH" "/opt/ros/noetic/include")
;;   :config
;;   (setq ccls-executable "~/.emacs.d/.lsp/ccls/build/ccls")
;;   :hook ((c-mode c++-mode objc-mode) .
;;          (lambda () (require 'ccls) (lsp))))

;; create c++ hook for starting lsp
(setq gdb-many-windows t)
(defun my-cpp-lsp-hook ()
  (require 'lsp)
  (lsp))

(add-hook 'c++-mode-hook 'my-cpp-lsp-hook)
(add-hook 'c++-ts-mode-hook 'my-cpp-lsp-hook)


(defun my-cpp-link-compile-commands ()
  (unless (file-exists-p (concat projectile-cached-project-root "compile-commands.json"))
    (shell-command (concat "cd " projectile-cached-project-root " && ln -s " projectile-cached-project-root "build/compile_commands.json " projectile-cached-project-root "compile_commands.json"))))

(add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$")

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

;;; Rust Mode
(use-package rust-mode
  :ensure t)

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

(unless (executable-find "gocode") (shell-command "go get golang.org/x/tools/gopls@latest"))
(setq exec-path (append exec-path '("~/.emacs.d/gopkgs/bin")))
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

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

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                  ,(rx (or "}" "]" "end"))                       ; Block end
                  ,(rx (or "#" "=begin"))                        ; Comment start
                  ruby-forward-sexp nil)))
;;; Python Mode
(use-package company-jedi
  :ensure t)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-extra-paths ["/opt/ros/melodic/lib/python2.7/dist-packages"])
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (setq lsp-python-ms-extra-paths ["/opt/ros/melodic/lib/python2.7/dist-packages"])
                          (lsp))))  ; or lsp-deferred



;; (use-package lsp-pyright
  ;; :ensure t
  ;; :init
  ;; (setq lsp-pyright-extra-paths '("/opt/ros/melodic/lib/python2.7/dist-packages"))
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-pyright)
  ;;                         (setq lsp-pyright-extra-paths '("/opt/ros/melodic/lib/python2.7/dist-packages"))
  ;;                         (lsp))))  ; or lsp-deferred
;; (defun my-python-mode-hook ()
  ;; (lsp))
;; (add-hook 'python-mode-hook 'my-python-mode-hook)

;;; XML
(setq nxml-slash-auto-complete-flag t)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
;; (add-hook 'nxml-mode-hook 'hs-minor-mode)

;;; Web Technologies
(use-package web-mode
  :ensure t)
(use-package rjsx-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
(setq web-mode-engines-alist
      '(("hugo" . ".*hugo.*html\\'")
        ("elixir" . "\\.heex\\'")))
(setq web-mode-enable-auto-closing t)
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))

(defun my-rjsx-hook ()
  (lsp-mode)
  )

(add-hook 'rjsx-mode-hook 'my-rjsx-hook)

;; (use-package company-tern
  ;; :ensure t
  ;; :init
  ;; (add-to-list 'company-backends 'company-tern))

;;; Yaml Mode
(use-package yaml
  :ensure t)

;;; Toml Mode
(use-package toml-mode
  :ensure t)

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

(use-package request
  :ensure t)

(defun roon-playpause()
  (interactive)
  (request "http://kota-server:7373/api/playpause"))

(defun roon-next()
  (interactive)
  (request "http://kota-server:7373/api/next"))

(defun roon-previous()
  (interactive)
  (request "http://kota-server:7373/api/previous"))

(defun music-playpause()
  (interactive)
  (cond ((equal musicplayer-backend "roon") (roon-playpause))
        ((equal musicplayer-backend "spotify") (spotify-toggle-play-pause))))

(defun music-next()
  (interactive)
  (cond ((equal musicplayer-backend "roon") (roon-next))
        ((equal musicplayer-backend "spotify") (spotify-next))))

(defun music-previous()
  (interactive)
  (cond ((equal musicplayer-backend "roon") (roon-previous))
        ((equal musicplayer-backend "spotify") (spotify-previous))))

(setq musicplayer-backend "roon")

(defun music-set-backend-roon()
  (interactive)
  (setq musicplayer-backend "roon"))

(defun music-set-backend-spotify()
  (interactive)
  (setq musicplayer-backend "spotify"))

(global-set-key (kbd "C-c <home>") 'music-playpause)
(global-set-key (kbd "<XF86AudioPlay>") 'music-playpause)
(global-set-key (kbd "C-c <prior>") 'music-next)
(global-set-key (kbd "<XF86AudioNext>") 'music-next)
(global-set-key (kbd "C-c <insert>") 'music-previous)
(global-set-key (kbd "<XF86AudioPrev>") 'music-previous)

(use-package pdf-tools
  :ensure t)

(when (not (package-installed-p 'pdf-tools))
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'pdf-tools)
  (pdf-tools-install))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;; Secrets
(use-package pass
  :ensure t)


;;; Linear App
(defmacro with-linear-call-post(graphql &rest body)
  `(progn
     (message ,graphql)
     (request
            "https://api.linear.app/graphql"
            :type "POST"
            :headers (list (cons "Content-Type" "application/json")
                           (cons "Authorization" (format "Bearer %s" (password-store-get "linear-user-token"))))
            :data ,graphql
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        ,@body)))))

(defmacro with-linear-get-me (&rest body)
  `(progn
     (with-linear-call-post
      (json-encode '(("query" . "{ viewer { id } }")))
      (let ((linear/user-info (json-read-from-string data)))
        ,@body))))

(defmacro with-linear-get-teams (&rest body)
  `(progn
     (with-linear-call-post
      (json-encode '(("query" . "{ teams { nodes { id name } } }")))
      (let ((linear/user-teams (json-read-from-string data)))
        ,@body))))

;; (with-linear-get-me
;;  (message (let-alist linear/user-info .data.viewer.id)))

;; (with-linear-get-teams
;; (let ((teams (append (let-alist linear/user-teams .data.teams.nodes) nil)))
;;     (message (car teams))))
 ;; (message (let-alist linear/user-info .data.viewer.id)))


;; (defmacro with-linear-get-me (&rest body)
;;   `(progn
;;      (linear--call-post
;;       (json-encode '(("query" . "{ viewer { id } }")))
;;       (cl-function
;;        (lambda (&key data &allow-other-keys)
;;          (message data)
;;          (let ((linear/user-info (json-read-from-string data)))
;;            ,@body))))))

;; (defmacro with-linear-get-my-issues (&rest body)
;;   `(progn
;;      (with-linear-get-me
;;       (linear--call-post
;;        (json-encode
;;         (list
;;          (cons "query"
;;                (format "{ user(id: \"%s\") { id name assignedIssues { nodes { id title url } } } }"
;;                        (let-alist linear/user-info .data.viewer.id)))))
;;        (let ((linear/issues (json-read-from-string data)))
;;          ,@body)))))

;; (with-linear-get-my-issues
;;  (message "%s" linear/issues))

;; (defun linear-get-user-issues(user-id)
;;   (interactive "sUser id")
;;   (linear--call-post
;;    (json-encode
;;     (list
;;      (cons "query"
;;            (format "{ user(id: \"%s\") { id name assignedIssues { nodes { id title url } } } }" user-id))))
;;    (cl-function
;;     (lambda (&key data &allow-other-keys)
;;       (setq linear--user-issues data)))))

;; (defun linear-teams ()
;;   (interactive)
;;   (with-demo-buffer
;;     (with-linear-get-teams
;;      (let ((teams (append (let-alist linear/user-teams .data.teams.nodes) nil))
;;            (inhibit-read-only t))
;;        (magit-insert-section (demo-files)
;;          (magit-insert-heading "Linear Teams\n")
;;          (message "teams: %s    %s" teams (length teams))
;;          (dolist (team teams)
;;            (magit-insert-section (demo-file team)
;;              (message "%s" team)
;;              (message "Item:: %s" (let-alist team .name))
;;              (insert (format "%s\n" (let-alist team .name))))))))))

;;; Misc
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Boston"
                                "Tokyo")))

;;; Start server
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))


(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p "~/.emacs.d/custom.el")
    (load-file custom-file))
(put 'downcase-region 'disabled nil)



(defun line_localization_setup ()
  (interactive)
  (let ((process-connection-type 't))
    (start-process-shell-command "roscore" "roscore-process" "rosdocker 'source /opt/ros/kinetic/setup.zsh && roscore'")
    (start-process-shell-command "rviz" "rviz-process" "rosdocker 'source /opt/ros/kinetic/setup.zsh && rviz'")
    (start-process-shell-command "rosbag" "rosbag-process"
                                 "rosdocker 'source /opt/ros/kinetic/setup.zsh && /home/kota/bin/rosbag_currtime_playback.py /home/kota/2021-12-07-22-47-27_back_and_forth_straight_line.bag'")))

(defun line_localization_kill ()
  (interactive)
  (kill-buffer "roscore-process")
  (kill-buffer "rviz-process")
  (kill-buffer "rosbag-process"))

;;; Transient

(use-package transient
  :ensure t)

(transient-define-prefix transient-org-find-bindings ()
  [("f" "find from all nodes" (lambda () (interactive) (org-roam-node-find)))]
  [("w" "find from work nodes" (lambda () (interactive) (org-roam-node-find t nil 'my/org-roam-work-only)))]
  [("p" "find from personal nodes" (lambda () (interactive) (org-roam-node-find t nil 'my/org-roam-personal-only)))]
  [("i" "find from web nodes" (lambda () (interactive) (org-roam-node-find t nil 'my/org-roam-web-only)))])

(transient-define-prefix transient-org-bindings ()
  "Org Interface"
  [("i" "insert roam node" (lambda () (interactive) (org-roam-node-insert)))]
  [("f" "find roam node" (lambda () (interactive) (transient-org-find-bindings)))]
  [("l" "list org roam" (lambda () (interactive) (org-roam-buffer-toggle)))]
  [("u" "org roam ui" (lambda ()
                        (interactive)
                        (unless org-roam-ui-mode (org-roam-ui-mode))
                        (org-roam-toggle-ui-xwidget)))]
  [("c" "copy org to clipboard" (lambda () (interactive) (copy-org-to-clipboard)))]
  [("s" "org sort" (lambda () (interactive) (org-sort)))])

; taken from https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs
(defun x11-yank-image-at-point-as-image ()
  "Yank the image at point to the X11 clipboard as image/png."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond (data
                 (with-temp-buffer
                   (insert data)
                   (call-shell-region
                    (point-min) (point-max)
                    "xclip -i -selection clipboard -t image/png")))
                (file
                 (if (file-exists-p file)
                     (start-process
                      "xclip-proc" nil "xclip"
                      "-i" "-selection" "clipboard" "-t" "image/png"
                      "-quiet" (file-truename file))))
                (t
                 (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))

(transient-define-prefix transient-file-bindings ()
  [("p" "copy file path to clipboard" (lambda () (interactive) (kill-new buffer-file-name)))]
  [("i" "copy current image" (lambda () (interactive) (x11-yank-image-at-point-as-image)))])

(transient-define-prefix transient-global-bindings ()
  "Global Interface"
  [("o" "org mode" (lambda () (interactive) (transient-org-bindings)))]
  [("f" "file operations" (lambda () (interactive) (transient-file-bindings)))])

(global-set-key (kbd "<XF86Launch8>") 'copilot-accept-completion) ; f17
(global-set-key (kbd "<XF86LaunchA>") 'transient-global-bindings) ; f19
(global-set-key (kbd "C-c o") 'transient-org-bindings)
