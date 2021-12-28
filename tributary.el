(require 'request)


(defcustom tributary-api-url nil "URL to the Confluence API.")



(define-derived-mode tributary-mode nxml-mode "Tributary"
  "Tributary is a major mode for editing and publishing Confluence pages."

  (setq-local tributary--id nil)
  (setq-local tributary--version-number nil)
  (setq-local tributary--title nil)
  (setq-local tributary--space-key nil)

  (define-key tributary-mode-map (kbd "C-c C-c") 'tributary-push)
  )


(defun tributary-pull-id (id)
  (interactive "sPage ID: ")
  (let* ((buf-name (tributary--buffer-name id))
         (prompt (format "Buffer %s already exists, overwrite?" buf-name))
         (buf (get-buffer buf-name)))
    (if buf
        (when (yes-or-no-p prompt)
          (progn
            (kill-buffer buf)
            (tributary--request-id id)))
      (tributary--request-id id)
      )))


;; (tributary-pull-id "1704722479")


(defun tributary--request-id (id)
  (let ((url (concat (file-name-as-directory tributary-api-url) id "?expand=body.storage,space,version")))
    (request url
      :auth "basic"
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (tributary--setup-edit-buffer data)
                    ))))))


(defun tributary--buffer-name (id)
  (format "*tributary*%s*" id))


(defun tributary--setup-current-buffer (data)
  (let-alist data
    (switch-to-buffer (tributary--buffer-name .id))
    (tributary-mode)
    (tributary--setup-local-vars data)
    (insert "<confluence>\n")
    (insert .body.storage.value)
    (insert "\n</confluence>\n")
    (tributary--set-schema)
    ;; (sgml-pretty-print (point-min) (point-max))
    )
  )

(defun tributary--setup-local-vars (data)
  (let-alist data
    (setq tributary--id .id)
    (setq tributary--version-number (1+ .version.number))
    (setq tributary--title .title)
    (setq tributary--space-key .space.key)
    (setq header-line-format (format "%s [%s]" tributary--title tributary--version-number))
    ))


(defun tributary--set-schema ()
  (let ((dir (file-name-directory (locate-library "tributary"))))
    (rng-set-schema-file (concat dir "confluence.rnc"))))


(defun tributary-push ()
  (interactive)
  (let ((url (concat (file-name-as-directory tributary-api-url) tributary--id))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (request url
      :auth "basic"
      :type "PUT"
      :sync t
      :data (json-encode `((id . ,tributary--id)
                           (type . "page")
                           (title . ,tributary--title)
                           (space (key . ,tributary--space-key))
                           (body (storage (value . ,text)
                                          (representation . "storage")))
                           (version (number . ,tributary--version-number))
                           ))
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (tributary--setup-local-vars data)
                  (message "Version %s pushed." (let-alist data .version.number))))
      :error (cl-function
              (lambda (&key data &allow-other-keys)
                (let-alist data
                  (message "ERROR %s: %s" .statusCode .message))))
      )))


(provide 'tributary)
