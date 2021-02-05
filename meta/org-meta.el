;; bootstrap quelpa and use-package:
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa '(use-package :fetcher git :url "https://github.com/jwiegley/use-package.git"))
(quelpa '(quelpa-use-package
          :fetcher git
          :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
;; install dependencies with use-package
(use-package org-resolve-deps :quelpa
  (org-resolve-deps :fetcher git
                    :url "https://github.com/EnigmaCurry/org-resolve-deps.git"))
;; load included unpackged functions, unless already installed (from upstream)
(unless (boundp 'unpackaged/org-export-html-with-useful-ids-mode)
  (load-file "../../meta/unpackaged.el"))
(unpackaged/org-export-html-with-useful-ids-mode)

(setq literate-k3s-buffer nil)
(let ((literate-k3s-buffer (current-buffer)))
  (defun literate-k3s-reload-config ()
    (message (concat "Reloading literate-k3s config: " (buffer-file-name literate-k3s-buffer)))
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
                         (eq major-mode 'org-mode)
                         (not (string-prefix-p
                               "tmp.build." (file-name-nondirectory (buffer-file-name (current-buffer))))))
                (message (buffer-file-name (current-buffer)))
                (literate-k3s-reload-config)
                (with-current-buffer (buffer-name literate-k3s-buffer)
                  (org-html-export-to-html)
                  ;;;; Tried to combine Org files in one export
                  ;;;; HOWEVER, all the header-args are stripped so this doesn't work:
                  ;; (let ((tmpfile
                  ;;        (concat (make-temp-name
                  ;;                 (concat default-directory "tmp.build."))
                  ;;                ".org")))
                  ;;   (org-export-to-file 'org tmpfile)
                  ;;   (with-current-buffer (find-file-noselect tmpfile)
                  ;;     (org-babel-tangle)))
                  ))))
  (add-hook 'org-babel-pre-tangle-hook
            (lambda ()
              (when (and (member default-directory literate-k3s-watch-dirs)
                         (eq major-mode 'org-mode)
                         (not (string-prefix-p
                               "tmp.build." (file-name-nondirectory (buffer-file-name (current-buffer))))))
                (literate-k3s-reload-config)))))


