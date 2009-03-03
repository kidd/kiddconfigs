;; Time-stamp: <2009-03-03 10:45:35 rgrau>

(add-hook 'before-save-hook 'time-stamp)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("/home/rgrau/EA.lib")))
 '(erc-port 8000)
 '(icicle-command-abbrev-alist nil)
 '(icicle-reminder-prompt-flag 0))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )



(global-set-key [\M-tab] 'hippie-expand)  
(add-to-list 'load-path "~/elisp")
(require 'tempo-snippets)
(tempo-define-snippet "c-for-it"
  '(> "for (" (p "Type: " type) "::iterator " (p "Iterator: " it) " = "
      (p "Container: " container) ".begin();" n>
      (s it) " != " (s container) ".end(); ++" (s it) ") {" > n> r n "}" >)
  "fori"
  "Insert a C++ for loop iterating over an STL container."
  nil)


(require 'tempo-c-cpp)
(require 'flymake)

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)


(add-to-list 'load-path "~/elisp")
(require 'tempo-snippets)

(add-hook 'find-file-hook 'flymake-find-file-hook)

(require 'doc-view)
(recentf-mode t)

(require 'linum)
(global-linum-mode 1)
(column-number-mode 1)
(global-font-lock-mode t)

;; automatically load cperl-mode for perl files
(fset 'perl-mode 'cperl-mode)

(setq recentf-auto-cleanup 'never) 
(recentf-mode 1)

(require 'remember-autoloads)
(global-set-key (kbd "C-c r") 'remember) ;; (3)


(defun wicked/remember-review-file ()
  ;;  "Open `remember-data-file'."
  (interactive)
  (find-file-other-window remember-data-file))
(global-set-key (kbd "C-c R") 'wicked/remember-review-file) ;; (4)


;; icicles
(add-to-list 'load-path "~/elisp/icicles")
(load-file "~/elisp/icicles/icicles.el")
 (icy-mode)
;;(icicle-toggle-fuzzy-completion)
;;(ido-mode)



(setq transient-mark-mode t)

(setq c-default-style "stroustrup")

(require 'yasnippet-bundle)

(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;(setq viper-mode t)                ; enable Viper at load time
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(require 'viper)                   ; load Viper
(require 'vimpulse)                ; load Vimpulse
(setq woman-use-own-frame nil)     ; don't create new frame for manpages
(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)

(defun comment-line ()
  (interactive)
  (save-excursion
    (comment-region (progn (beginning-of-line) (point))
		    (progn (end-of-line) (point)))))

(global-set-key (kbd "C-;") 'comment-line)
(show-paren-mode t)

;; (color-theme-dark-blue2)
(color-theme-tty-dark)
;(iswitchb-mode)				
(ido-mode)
(setq ido-enable-flex-matching t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/work.org"
                            "~/org/school.org"
                            "~/org/home.org"))

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(require 'doc-mode)
(add-hook 'c++-mode-hook 'doc-mode)
(add-hook 'c-mode-hook 'doc-mode)


(setq hl-line-sticky-flag t)
(hl-line-mode t)
(global-hl-line-mode)

(setq org-log-done t)			
(tool-bar-mode -1)
(display-time-mode)
(setq european-calendar-style 't)
;(define-key viper-insert-global-user-map (kbd "jk") 'viper-intercept-ESC-key)

;timeclock
;project


(defadvice show-paren-function (after show-matching-paren-offscreen activate)
"If the matching paren is offscreen, show the matching line in the                               
echo area. Has no effect if the character before point is not of                                   
the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point                               
    ;; is a close parentheses type character. Otherwise, there's not                               
    ;; really any point, and `blink-matching-open' would just echo                                 
    ;; "Mismatched parentheses", which gets really annoying.                                       
    (if (char-equal (char-syntax (char-before (point))) ?\))
        (setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
        (message matching-text))))
;;(featurep 'x-win)


;(defslime-start sbcl "/usr/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
;(add-to-list 'load-path "~/hacking/lisp/slime/") ; your SLIME directory
(require 'slime)
(slime-setup)
(c-subword-mode 1)

(defun factorial (x)
  (if (= x 1)
      1
    (* (factorial (1- x)) x)))


; latex

(setq flyspell-default-dictionary "catala-tex")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
