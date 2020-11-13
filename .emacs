(add-to-list 'exec-path "/usr/bin/")
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))

                                        ; Loading packages. Needs `use-package` installed.

;; Fixing signature check failure.
;; Uncomment and evaluate this:
;; (setq package-check-signature nil)
;; Then install package: gnu-elpa-keyring-update
;; Finally, uncomment (setq package-check...) and restart.


(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(use-package highlight-numbers :ensure t)
(use-package highlight-quoted :ensure t)
(use-package org :ensure t)
(use-package org-bullets :ensure t)
(use-package ox-twbs :ensure t)
(use-package ace-window :ensure t)
;; (use-package recentf :ensure t)
(use-package tramp :ensure t)
(use-package bm :ensure t)
(use-package flycheck :ensure t)
(use-package ledger-mode :ensure t)
(use-package flycheck-ledger :ensure t)
(use-package iedit :ensure t)
;; (use-package neotree :ensure t)
(use-package electric :ensure t)
(use-package go-mode :ensure t)
;; (use-package xah-find :ensure t)
(use-package bison-mode :ensure t)
(use-package spacemacs-theme :defer t :ensure t)
(use-package theme-changer :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package helm-descbinds :ensure t)
;; (use-package disable-mouse :ensure t)
(use-package auctex :defer t :ensure t)
;; (use-package slime :defer t :ensure t)
(use-package sly :defer t :ensure t)
(use-package transpose-frame :defer t :ensure t)
(use-package magit :defer t :ensure t)

                                        ; Emacs-specific configuration.

;; LaTeX configuration.
(setq TeX-PDF-mode t)
(setq latex-run-command "pdflatex")


;; cl includes some required definitions by w3m.
(require 'cl)

;; Line numbers separation.
(setq linum-format "%4d \u2502 ")

;; Visual line mode.
(global-visual-line-mode t)

;; Remove ^M in files containing mixed UNIX and DOS line endings.
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

;; Top-bottom document navigate.
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)

;; List navigation.
(global-set-key (kbd "M-p") 'backward-list)
(global-set-key (kbd "M-n") 'forward-list)

;; Indentation.
(global-set-key (kbd "M-q") 'indent-pp-sexp)

;; Ergonomic backspace.
(global-set-key (kbd "C-h") (kbd "<backspace>"))

;; Mark defun.
(global-set-key (kbd "M-h") 'mark-defun)

;;; Prefer backward-kill-word over backspace.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Comment region.
(global-set-key (kbd "C-c #") 'comment-region)
;; Uncomment region.
(global-set-key (kbd "C-c $") 'uncomment-region)

;; Override mail.
(global-set-key (kbd "C-x m") 'execute-extended-command)

;; Disable evil auto copy selected text.
;; (delete-selection-mode)
;; (setq x-select-enable-primary nil)
;; (setq x-select-enable-clipboard nil)

;; Show OS menu bar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Linum.
(global-linum-mode 1)

;; Recenter.
;; (global-set-key (kbd "C-'") 'recenter)

;; Display time
(setq display-time-day-and-date t)
(display-time)

;; Display column number.
(setq column-number-mode t)

;; Change font size
(setq resize-mini-windows nil)
(set-face-attribute 'default nil :height 100)

;; Font
(set-frame-font "Consolas 13" nil t)

;; Set Locale
(setenv "LANG" "en_US.UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Avoid asking "are you sure you want to save?"
(defun ask-user-about-supersession-threat (fn)
  "Blatantly ignore files that changed on disk."
  )

(defun ask-user-about-lock (file opponent)
  "Always grab lock."
  t)

;; ?? in line number.
(setq line-number-display-limit-width 2000000)

;; Insert date in text
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
		   ;;((not prefix) "%a %b %d  %H:%M:%S")
		   ((not prefix) "[%Y-%m-%d %a %H:%M]")
		   ((equal prefix '(4)) "%Y-%m-%d")
		   ((equal prefix '(16)) "%A, %d. %B %Y")))
	  (system-time-locale "en_US"))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; Cursor type and color
(setq-default cursor-type 'box)
(set-cursor-color "#fff")

;; Automatically refresh files/buffers.
(global-auto-revert-mode t)

;; Cursor delay.
(setq blink-cursor-delay 0)

;; Auto-balance buffers.
(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))

(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))

(defadvice delete-window (after restore-balance activate)
  (balance-windows))

;; Open in same window.
(add-to-list 'same-window-buffer-names "*xah-find output*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*grep*")

;; Temporarily maximize a buffer fullscreen.
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
	   (progn
	     (jump-to-register '_)
	     (balance-windows))
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows)
	   (balance-windows))))

(global-set-key (kbd "<f9>") 'toggle-maximize-buffer)

;; Disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable mark ring.
(setq mark-ring-max 0)
(setq global-mark-ring-max 0)
(global-unset-key (kbd "C-x C-x"))

;; Problem with certain fonts.
(setq inhibit-compacting-font-caches t)

;; Lemonade; clipboard client.
(defun lemonade-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "lemonade copy")
  )
(defun lemonade-paste ()
  (interactive)
  (insert (shell-command-to-string "lemonade paste"))
  )
(global-set-key (kbd "<f5>") 'lemonade-copy)
(global-set-key (kbd "<f6>") 'lemonade-paste)

;; Changing set-mark.
(global-set-key (kbd "C-t") 'set-mark-command)

;; (Un)Commenting regions.
(global-set-key (kbd "C-x C-q") 'comment-region)
(global-set-key (kbd "C-x C-j") 'uncomment-region)

                                        ; Package-specific configuration.

;; electric-pair-mode.
(electric-pair-mode t)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) 
    (self-insert-command 1)))

(add-hook 'lisp-mode-hook
          (lambda ()
	    (highlight-quoted-mode t)
	    (highlight-numbers-mode t)
            (define-key lisp-mode-map "\"" 'electric-pair)
            (define-key lisp-mode-map "(" 'electric-pair)
            (define-key lisp-mode-map "[" 'electric-pair)
            (define-key lisp-mode-map "{" 'electric-pair)))

;; org-mode.
(with-eval-after-load 'org
  ;; (define-key org-mode-map [(control c) (control c)] 'org-global-cycle)
  (define-key org-mode-map [(control c) (control n)] 'org-insert-heading)
  (define-key org-mode-map [(control up)] 'org-metaup)
  (define-key org-mode-map [(control down)] 'org-metadown)
  (define-key org-mode-map [(control left)] 'org-metaleft)
  (define-key org-mode-map [(control right)] 'org-metaright)
  (org-bullets-mode t)
  )
(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-twbs)
(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
(add-to-list 'org-latex-packages-alist '("cache=false" "minted"))
;; NOTE: minted requires pygments to be installed.
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-src-fontify-natively t)
(require 'ob-lisp)
(setq org-latex-prefer-user-labels t)

;; Magit.

;; Transpose frame.
(global-set-key (kbd "C-x C-x 1") 'transpose-frame)

;; Settting up slime.
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq sly-contribs '(sly-fancy))

(add-hook 'go-mode-hook
	  (defun golang-sanitize-bindings ()
	    "Removes Golang's keybindings."
	    (cond ((boundp 'go-mode-map)
		   (define-key go-mode-map (kbd "C-c C-a") nil)
		   (message "go-mode keybinding on C-c x has been sanitized"))
		  ('t (message "go-mode keybindings not sanitized")))))

(add-hook 'sly-mode-hook
	  (defun sly-sanitize-bindings ()
	    "Removes SLY's keybindings."
	    (cond ((boundp 'sly-mode-map)
		   (define-key sly-mode-map (kbd "C-c C-e") nil)
		   (define-key sly-editing-mode-map (kbd "M-p") nil)
		   (define-key sly-editing-mode-map (kbd "M-n") nil)
		   (define-key sly-editing-mode-map (kbd "M-,") nil)
		   (define-key sly-editing-mode-map (kbd "M-.") nil)
		   (message "sly keybinding on C-c x has been sanitized"))
		  ('t (message "sly keybindings not sanitized")))))

;; Visible bookmarks (bm).
;; For terminal environment.
(global-set-key (kbd "C-c o") 'bm-toggle)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-o") 'bm-toggle)
;; For terminal environment.
(global-set-key (kbd "C-c a") 'bm-previous)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-a") 'bm-previous)
;; For terminal environment.
(global-set-key (kbd "C-c e") 'bm-next)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-e") 'bm-next)


;; Ace-window.
(global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Tramp enables you to use sudo
(require 'tramp)
(setq tramp-default-method "scp")

;; Saves a list of recent opened files
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-auto-cleanup 'never)

;; (defun recentf-open-files-compl ()
;;   (interactive)
;;   (let* ((all-files recentf-list)
;; 	 (tocpl (mapcar (function
;; 			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
;; 	 (prompt (append '("Recent File name: ") tocpl))
;; 	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;     (find-file (or (cdr (assoc fname tocpl))
;; 		   fname))))

;; (global-set-key "\C-x\C-r" 'recentf-open-files-compl)

;; Use xetex instead of pdftex.
;; (setq latex-run-command "xetex")

;; Show parenthesis.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Neotree.
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-smart-open t)

;; Flycheck.
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Iedit.
(require 'iedit)
(global-set-key (kbd "C-d") 'iedit-mode)
(put 'narrow-to-region 'disabled nil)

;; Disable mouse.
;; (global-disable-mouse-mode)

;; Rainbow-delimiters for major programming modes.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Automatically change theme depending on daytime.
(setq calendar-location-name "Tijuana, MX")
(setq calendar-latitude 32.534851)
(setq calendar-longitude -117.043457)
(require 'theme-changer)
(change-theme 'spacemacs-dark 'spacemacs-dark)

                                        ; Extra.

;; Use Golang syntax with CXGO programs
(add-to-list 'auto-mode-alist '("\\.cx\\'" . go-mode))

;; Copy `diff` results without `-` or `+` at the beginning of line.
(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

(global-set-key (kbd "C-x M-w") 'copy-diff-region)

(provide '.emacs)
;;; .emacs ends here
