;; .emacs-bootstrap.el
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg '(ox-hugo))
  (unless (package-installed-p pkg)
    (package-install pkg)))
;; 確保載入 ox-hugo
(require 'ox-hugo nil t)
;; 此行可以讓 build-hugo.el 找到 repo 根（CI runner 的 default-directory 已是 repo 根）
;; 載入並執行 build 檔案（batch 時會執行其中的 main 函式或主程式）
;; build-hugo.el 應該定義並在載入時執行主要流程
(load-file (expand-file-name "build-hugo.el" default-directory))
