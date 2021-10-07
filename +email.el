;;; ../../dotfiles/doom/+email.el -*- lexical-binding: t; -*-
;; mu4e setup
;;(after! mu4e
;;  (setq mu4e-root-maildir (expand-file-name "~/.local/share/mail/essb")
;;        mu4e-get-mail-command "mbsync -a -c \"$XDG_CONFIG_HOME/isync/mbsyncrc\""
;;        mu4e-index-update-in-background t
;;        mu4e-use-fancy-chars t
;;        mu4e-view-show-addresses t
;;        mu4e-view-show-images t
;;        mu4e-compose-format-flowed t
;;        mu4e-compose-signature-auto-include nil
;;        mu4e-view-use-gnus t
;;        mu4e-change-filenames-when-moving t
;;        message-send-mail-function 'smtpmail-send-it
;;        message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
;;        message-citation-line-function 'message-insert-formatted-citation-line
;;        message-kill-buffer-on-exit t
;;        org-mu4e-convert-to-html t)
;;  (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
;;  (add-hook 'mu4e-compose-mode-hook (lambda() (use-hard-newlines -1))))

;; Email alert
;;(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; Setup email account
;;(set-email-account! "essb"
;;                    '((mu4e-sent-folder                 .       "/essb/Sent")
;;                      (mu4e-drafts-folder               .       "/essb/Drafts")
;;                      (mu4e-trash-folder                .       "/essb/Trash")
;;                      (mu4e-refile-folder               .       "/essb/INBOX")
;;                      (message-send-mail-function       .       smtpmail-send-it)
;;                      (smtpmail-smtp-user               .       "45995wsp@eur.nl")
;;                      (smtpmail-smtp-server             .       "localhost")
;;                      (smtpmail-smtp-service            .       1025)
;;                      (smtpmail-stream-type             .       nil)
;;                      (user-mail-address                .       "spekkink@essb.eur.nl")
;;                      (mu4e-update-interval             .       300))
;;                    t)

(after! org-msg
  (setq org-msg-default-alternatives nil))
