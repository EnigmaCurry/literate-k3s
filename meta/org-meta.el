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

;; Remember the buffer that loaded org-meta:
;; NOTE: this is a global variable, so this only supports having one cluster
;; file open at a time. I tried setq-local but it didn't work ??
(setq literate-k3s-buffer nil)
(let ((literate-k3s-buffer (current-buffer)))
  (defun literate-k3s-reload-config ()
    (message (concat "Reloading literate-k3s config: " (buffer-file-name literate-k3s-buffer)))
    (org-babel-lob-ingest (buffer-file-name literate-k3s-buffer)))

  (literate-k3s-reload-config)
  (if (boundp 'literate-k3s-watch-dirs)
      (progn
        (add-to-list 'literate-k3s-watch-dirs default-directory)
        (add-to-list 'literate-k3s-watch-dirs (concat default-directory "meta/")))
    (setq literate-k3s-watch-dirs
          (list default-directory (concat default-directory "meta/"))))
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (member default-directory literate-k3s-watch-dirs)
                         (eq major-mode 'org-mode)
                         (not (equal ".org-resolve-deps.org" (file-name-nondirectory (buffer-file-name (current-buffer))))))
                (message (buffer-file-name (current-buffer)))
                (literate-k3s-reload-config)
                (with-current-buffer (buffer-name literate-k3s-buffer)
                  (org-html-export-to-html)
                  ;;;; I TRIED to combine Org files into one export ...
                  ;;;; HOWEVER, all the header-args will be strip searched
                  ;;;; and left naked (all header args missing from the export),
                  ;;;; so this DOESN'T work:
                  ;;;;; https://dev.to/enigmacurry/comment/1b75j
                  ;; (let ((tmpfile
                  ;;        (concat (make-temp-name
                  ;;                 (concat default-directory "tmp.build."))
                  ;;                ".org")))
                  ;;   (org-export-to-file 'org tmpfile)
                  ;;   (with-current-buffer (find-file-noselect tmpfile)
                  ;;     (org-babel-tangle)))
                  ;;;; INSTEAD of this I'm using https://github.com/EnigmaCurry/org-resolve-deps
                  ;;;; which is my own fork of https://github.com/hkjels/org-resolve-deps
                  ;;;; The author hkjels preferred that exports from INCLUDES
                  ;;;; exported relative to the source file, which is the
                  ;;;; OPPOSITE of what I wanted, so my version exports relative to the document doing the INCLUDE.
                  ;;;; SEE https://github.com/EnigmaCurry/org-resolve-deps/commit/ab4170dd43cc3de7be60bb2480d9869dab0e8182
                  (org-resolve-deps-tangle)
                  ))))
  (add-hook 'org-babel-pre-tangle-hook
            (lambda ()
              (when (and (member default-directory literate-k3s-watch-dirs)
                         (eq major-mode 'org-mode)
                         (not (eq ".org-resolve-deps.org" (file-name-nondirectory (buffer-file-name (current-buffer)))))) 
               (literate-k3s-reload-config)))))


