;;; ../../dotfiles/doom/+betterDefaults.el -*- lexical-binding: t; -*-

;; Set line-number style
(setq display-line-numbers-type 'relative)
;; Exit insert mode by pressing j twice quickly
(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)
(setq evil-escape-key-sequence nil)

;; doom-mode-line stuff
(setq doom-modeline-enable-word-count t)

;; Set the system time display mode
(setq system-time-locale "C")
;; When opening the file, the cursor is automatically positioned to the position where it stayed last time
(save-place-mode 1)
;; Coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;; No additional confirmation required when closing emacs
(setq confirm-kill-emacs nil)

;;Projectile
;;projectile-project-search-path '("~/Project/" "~/dotfiles/" "~/blog/content-org/")

;;(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
;;(define-key vterm-mode-map [return] #'vterm-send-return)
;;
;;
;;
(after! (vterm evil-collection)
  (add-hook!
   'vterm-mode-hook
   ;; evil-collection for vterm overrided some keymaps defined by tmux-pane
   (evil-collection-define-key 'insert 'vterm-mode-map
     (kbd "C-h") (lambda () (interactive) (tmux-pane--windmove
                                      "left"
                                      "tmux select-pane -L"))
     (kbd "C-j") (lambda () (interactive) (tmux-pane--windmove
                                      "down"
                                      "tmux select-pane -D"))
     (kbd "C-k") (lambda () (interactive) (tmux-pane--windmove
                                      "up"
                                      "tmux select-pane -U"))
     (kbd "C-l") (lambda () (interactive) (tmux-pane--windmove
                                      "right"
                                      "tmux select-pane -R")))
   ;; change keymap to toggle sending escape to vterm
   (evil-collection-define-key '(normal insert) 'vterm-mode-map
     (kbd "C-c") 'vterm--self-insert
     ;; for CLI emacs
     (kbd "ESC <escape>") 'evil-collection-vterm-toggle-send-escape
     ;; for GUI emacs
     (kbd "M-<escape>") 'evil-collection-vterm-toggle-send-escape)
   ;; send escape to vterm by default
   (evil-collection-vterm-toggle-send-escape)))
