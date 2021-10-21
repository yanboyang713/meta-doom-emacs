;;; ../../dotfiles/doom/+email.el -*- lexical-binding: t; -*-
;; mu4e setup
(after! mu4e
  (setq mu4e-root-maildir (expand-file-name "~/MailDir")
        mu4e-get-mail-command "mbsync -a -c  ~/.config/mu4e/mbsyncr/mbsyncrc"
        ;;mu4e-index-update-in-background t
        mu4e-use-fancy-chars t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-compose-format-flowed t
        mu4e-compose-signature-auto-include t
        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir "~/Downloads"
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        ;;message-send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail

        message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-kill-buffer-on-exit t
        org-mu4e-convert-to-html t)
  (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
  (add-hook 'mu4e-compose-mode-hook (lambda() (use-hard-newlines -1))))

;; Email alert
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; Setup email account
(set-email-account! "gmail"
                    '((mu4e-sent-folder                 .       "/gmail/Sent")
                      (mu4e-drafts-folder               .       "/gmail/Drafts")
                      (mu4e-trash-folder                .       "/gmail/Trash")
                      (mu4e-refile-folder               .       "/gmail/INBOX")
                      ;;(message-send-mail-function       .       smtpmail-send-it)
                      (smtpmail-smtp-user               .       "yanboyang713@gmail.com")
                      (user-mail-address                .       "yanboyang713@gmail.com")
                      (mu4e-compose-signature . "---\nBoyang Yan")
                      (mu4e-update-interval             .       300))
                    t)

(after! org-msg
  (setq org-msg-default-alternatives nil))
