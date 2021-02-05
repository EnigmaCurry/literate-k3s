(setq-local literate-k3s-buffer (current-buffer))
(defun literate-k3s-reload-config ()
  (org-babel-lob-ingest (buffer-file-name literate-k3s-buffer)))
(literate-k3s-reload-config)
(if (boundp 'literate-k3s-watch-dirs)
    (progn
      (add-to-list 'literate-k3s-watch-dirs default-directory)
      (add-to-list 'literate-k3s-watch-dirs (concat default-directory "meta/"))
      )
  (setq literate-k3s-watch-dirs
        (list default-directory (concat default-directory "meta/"))))
(add-hook 'after-save-hook
          (lambda ()
            (when (and (member default-directory literate-k3s-watch-dirs)
                       (eq major-mode 'org-mode))
              (literate-k3s-reload-config)
              (with-current-buffer literate-k3s-buffer
                (org-html-export-to-html)))))
(add-hook 'org-babel-pre-tangle-hook
          (lambda ()
            (when (and (member default-directory literate-k3s-watch-dirs)
                       (eq major-mode 'org-mode))
              (literate-k3s-reload-config))))


