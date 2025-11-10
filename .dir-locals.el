;; .dir-locals.el
((nil . ((eval . (let ((base-dir (locate-dominating-file default-directory ".dir-locals.el")))
                  (setq-local
                   org-roam-directory (expand-file-name "org-content" base-dir))))
         (eval . (setq-local
                  org-roam-db-location (expand-file-name "org-roam.db"
                                                         org-roam-directory))))))                                                         (file-name-directory (or load-file-name org-roam-directory))))))))
