;; build-hugo.el
;; 可在本機或 CI 以 emacs -Q --batch -l .emacs-bootstrap.el 執行
;; 註：請確保 .emacs-bootstrap.el 或您的環境中已載入 ox-hugo 及 org-element
(require 'subr-x)
(require 'org)
(require 'json)
(require 'ox-hugo) ;; 確保載入 ox-hugo
(require 'cl-lib)  ;; 載入 cl-lib 以取得 let* (如果你的環境需要)

;; 調整下面路徑到你 repo 的實際目錄（相對於 repo 根）
(let* ((repo-root (file-name-as-directory (or (file-name-directory (or load-file-name default-directory)) default-directory)))
       (org-dir (expand-file-name "org-content/" repo-root))     ;; 你的 org 檔放這
       (hugo-site-root (expand-file-name "hugo-site/" repo-root)) ;; Hugo site root
       (hugo-content-dir (expand-file-name "content/stories/" hugo-site-root))
       (hugo-data-dir (expand-file-name "data/" hugo-site-root)))
  (message "repo-root=%s" repo-root)
  (make-directory hugo-content-dir t)
  (make-directory hugo-data-dir t)

  ;; helper
  (defun bh/read-file (f) (with-temp-buffer (insert-file-contents f) (buffer-string)))
  
  (defun bh-get-slug (file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (or (and (re-search-forward "^#\\+hugo_slug:[ \t]*\\(.+\\)$" nil t) (string-trim (match-string 1)))
          (file-name-base file))))
  
  (defun bh-get-title (file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (or (and (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t) (string-trim (match-string 1)))
          (file-name-base file))))
  
  (defun bh-get-id (file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; 匹配 :ID: 屬性
      (when (re-search-forward "^[ \t]*:ID:[ \t]*\\(.*\\)$" nil t) (string-trim (match-string 1)))))

  ;; collect files & metas
  (let* ((files (directory-files-recursively org-dir "\\.org$"))
         (metas (mapcar (lambda (f) (list :file f :slug (bh-get-slug f) :title (bh-get-title f) :id (bh-get-id f))) files))
         (id->slug (let ((h (make-hash-table :test 'equal))) (dolist (m metas) (when (plist-get m :id) (puthash (plist-get m :id) (plist-get m :slug) h))) h))
         (slug-map (let ((h (make-hash-table :test 'equal))) (dolist (m metas) (puthash (plist-get m :slug) m h)) h))
         (backlinks (make-hash-table :test 'equal)))

    ;; parse outgoing links for each file
    (dolist (m metas)
      (let* ((file (plist-get m :file))
             (slug (plist-get m :slug))
             (title (plist-get m :title)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((parse (org-element-parse-buffer)))
            (org-element-map parse 'link
              (lambda (link)
                (let ((type (org-element-property :type link))
                      (path (org-element-property :path link)))
                  (cond
                   ((and path (string= type "file"))
                    ;; file link -> map to slug by basename
                    (let ((tgt (file-name-base path)))
                      (when tgt
                        (let ((lst (gethash tgt backlinks)))
                          ;; ⚠️ 修正：將 plist 替換為 alist，並使用標準的 cons 構建
                          (let ((new-backlink (list (cons 'title title) (cons 'url (concat "/stories/" slug "/")))))
                            (puthash tgt (cons new-backlink lst) backlinks))))))
                   
                   ((and path (string= type "id"))
                    ;; id link -> use id->slug map if exists
                    (let ((tgt-id path)
                          (tgt-slug (gethash path id->slug)))
                      (when tgt-slug
                        (let ((lst (gethash tgt-slug backlinks)))
                          ;; ⚠️ 修正：將 plist 替換為 alist，並使用標準的 cons 構建
                          (let ((new-backlink (list (cons 'title title) (cons 'url (concat "/stories/" slug "/")))))
                            (puthash tgt-slug (cons new-backlink lst) backlinks)))))
                   
                   (t nil)))))))))
    
    ;; write backlinks.json
    (let ((out (make-hash-table :test 'equal)))
      (maphash (lambda (k v) (puthash k v out)) backlinks)
      ;; ⚠️ 修正：直接將 hash table 轉換為 alist (json-encode 支援 alist 作為 JSON Object)
      (let ((json-obj (let (acc) (maphash (lambda (k v) (push (cons k v) acc)) out) acc)))
        (with-temp-file (expand-file-name "backlinks.json" hugo-data-dir)
          (insert (json-encode json-obj)))))
    (message "backlinks.json written to %s" (expand-file-name "backlinks.json" hugo-data-dir))

    ;; Export each org via ox-hugo into hugo-site (page bundle)
    ;; Configure ox-hugo base & section to put into content/stories/<slug>/index.md
    (setq org-hugo-base-dir hugo-site-root)
    (setq org-hugo-default-section "stories")
    
    ;; export each
    (dolist (m metas)
      (let ((file (plist-get m :file))
            (slug (plist-get m :slug)))
        (message "Exporting %s -> slug %s" file slug)
        ;; open file and call ox-hugo export (non-interactive)
        (with-current-buffer (find-file-noselect file)
          ;; ensure slug is in front-matter if you want: add #+hugo_slug: ... is preferred in file already
          (org-hugo-export-wim-to-md)) ;; will create page bundle under hugo-site/content/stories/<slug>/
        (message "Exported %s" file)))
    (message "All done."))))
