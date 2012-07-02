;;; trindle.el - Simple Emacs LISP management extension
;;
;; Copyright (C) 2012 Daichi Hirata
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Author: Daichi Hirata <daichi.hirat@gmail.com>
;;
;;; Commentary:
;;
;; 'trindle.el' referred to "bundler" of programming language ruby, and created it.
;; Unlike package management, only the function of simple management is offered.
;; These manage that in which I have installed what from init.el(or any config file).
;;
;;;  TODO:
;;
;;     - Write a Sample and Useage.
;;     - Refactoring of duplication function.
;;     - Byte compile processing after installation is added.
;;
;;; Samples: It is temporary now :p
;;
;;   (require 'trindle)
;;   (setq trindle-dir "~/.emacs.d/elisp/trindle")
;;
;;   (trindle:packages
;;     (:type "github"    :name "ejmr/php-mode")
;;     (:type "github"    :name "defunkt/coffee-mode")
;;     (:type "github"    :name "daic-h/trindle")
;;     (:type "emacswiki" :name "auto-complete")
;;     (:type "emacswiki" :name "ruby-block")
;;     (:type "http"      :url  "http://www.emacswiki.org/emacs/download/anything.el"))
;;
;;   (trindle:initialize)
;;   (trindle:install)
;;   (trindle:install!)
;;   (trindle:update)
;;   (trindle:clear)
;;   (trindle:remove)
;;
;;--------------------------------------------------------------------

;;; Code:
(eval-when-compile (require 'cl))
(require 'deferred)

(defgroup trindle nil
  "trindle"
  :group 'trindle)

(defcustom trindle-dir "~/.emacs.d/trindle"
  "Path where to install the packages."
  :type 'directory
  :group 'trindle)

(defcustom trindle-load-packages
  t
  "Reads the init file on startup"
  :type 'boolean
  :group 'trindle)

(defcustom trindle-byte-compile
  t
  "Byte compile is performed at the time of download and update."
  :type 'boolean
  :group 'trindle)

(defcustom trindle-emacswiki-base-url
  "http://www.emacswiki.org/emacs/download/%s.el"
  "The download URL of emacswiki"
  :type 'string
  :group 'trindle)

(defcustom trindle-github-base-url
  "https://github.com/%s.git"
  "The download URL of Github"
  :type 'string
  :group 'trindle)

(defcustom trindle-result-buffer
  "*trindle log*"
  "Name of buffer for async processing result"
  :type 'string
  :group 'trindle)

(defconst trindle-methods
  '(:github    (:install trindle-github-clone
                :update  trindle-github-pull)
    :emacswiki (:install trindle-emacswiki-install
                :update  trindle-emacswiki-update)
    :git       (:install trindle-github-clone
                :update  trindle-github-pull)
    :http      (:install trindle-http-install
                :update  trindle-http-update)
    :svn       (:install trindle-svn-checkout
                :update  trindle-svn-update))
    "The list of methods according to action.")

(defconst trindle-excepts-path-rexp
  "^\\.\\|\\.elc$\\|^test$\\|^tests$\\|^sample$"
  "It excepts from `trindle-get-recuresive-path-list.'")

(defvar trindle-packages nil
  "Holding the list of packages.")

(defmacro trindle:packages (&rest packages)
  "Macro for defining a package briefly."
  `(dolist (package '(,@packages))
     (add-to-list 'trindle-packages package)))

(defvar trindle-load-path nil
  "Holding the list of packages load path.")

(defun trindle-init-file ()
  "The configuration file for adding the load path"
  (concat (file-name-as-directory trindle-dir) ".trindle.el"))

(defun trindle-get-package-name (url_or_repo)
  "Get package name by URL or repository name."
  (file-name-sans-extension (file-name-nondirectory url_or_repo)))

(defun trindle-get-install-dir (package-name)
  "Get install dir by package name."
  (concat (file-name-as-directory trindle-dir) package-name))

(defun trindle-get-install-path (package-name)
  "Get install file path by package name."
  (let ((install-dir (trindle-get-install-dir package-name)))
    (concat (file-name-as-directory install-dir) (concat package-name ".el"))))

(defun trindle-get-method (type action)
  "The method according to action is returned. "
  (plist-get (plist-get trindle-methods (intern (concat ":" type))) action))

(defun trindle-get-recuresive-path-list (file-list)
  "Get file path list recuresively."
  (let ((path-list))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (loop for x in file-list do
          (if (file-directory-p x)
              (setq path-list
                    (append
                     (trindle-get-recuresive-path-list
                      (remove-if (lambda (path)
                                   (string-match trindle-excepts-path-rexp (file-name-nondirectory path)))
                                 (directory-files x t)))
                     path-list))
            (add-to-list 'path-list x )))
    path-list))

(defun trindle-installed-pkg-list ()
  "List of the packages installed is returned."
  (when (file-directory-p trindle-dir)
    (remove-if '(lambda (package) (string-match "^\\.\\|trindle" package))
               (directory-files trindle-dir))))

(defun trindle-registered-pkg-list ()
  "List of the packages registered is returned."
  (let (result)
    (dolist (package trindle-packages result)
      (let* ((name (plist-get package :name))
             (url  (plist-get package :url))
             (package-name (trindle-get-package-name (or name url))))
        (add-to-list 'result package-name)))))

(defun trindle-make-task-list (action)
  "The list of processings for every package is returned."
  (mapcar '(lambda (package)
             (lexical-let* ((package package)
                            (type (plist-get package :type))
                            (method (trindle-get-method type action)))
               (lambda () (funcall method package)))) trindle-packages))

(defun trindle-add-to-load-path (paths)
  "It adds to a load bus recursively."
  (let (path)
    (dolist (path paths)
      (let ((default-directory path))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(defun trindle-write-load-path (install-path)
  "Writing the load path in the configuration file"
  (let ((install-full-path (expand-file-name install-path)))
    (add-to-list 'trindle-load-path install-path)
    (append-to-file
     (format "(add-to-list 'trindle-load-path \"%s\")\n" install-full-path)
     nil (trindle-init-file))))
    ;;(trindle-message "add load path \"%s\"" install-path)))

(defun trindle-delete-load-path (install-path)
  "Remove the load path from the configuration file"
  (when (file-exists-p (trindle-init-file))
    (with-temp-buffer
      (let ((install-full-path (expand-file-name install-path))
            after-save-hook)
        (insert-file-contents-literally (trindle-init-file))
        (goto-char (point-min))
        (delete-matching-lines install-full-path)
        (write-file (trindle-init-file))
        (kill-buffer)))))
      ;;(trindle-message "remove load path \"%s\"" install-path)))))

(defun trindle-message (&rest string)
  "Output to the buffer for `trindle-result-buffer'"
  (with-current-buffer (get-buffer-create trindle-result-buffer)
    (goto-char (point-max))
    (insert (concat (apply 'format string) "\n"))))

(defun trindle:initialize ()
  "The path of a package is added to a load path."
  (interactive)
  (when (file-exists-p (trindle-init-file))
    (load (trindle-init-file))
    (trindle-add-to-load-path trindle-load-path)))

(defun trindle:install ()
  (interactive)
  (let ((task (trindle-make-task-list :install)))
    (deferred:$
      (deferred:next
        (lambda () (trindle-message "Trindle Install START")))
      (deferred:loop (reverse task) 'funcall)
      (deferred:nextc it
        (lambda () (trindle:initialize)))
      (deferred:nextc it
        (lambda ()
          (display-buffer trindle-result-buffer)
          (trindle-message "Trindle Install END"))))))

(defun trindle:update ()
  (interactive)
  (let ((task (trindle-make-task-list :update)))
    (deferred:$
      (deferred:next
        (lambda () (trindle-message "Trindle Update START")))
      (deferred:loop (reverse task) 'funcall)
      (deferred:nextc it
        (lambda () (trindle:initialize)))
      (deferred:nextc it
        (lambda ()
          (display-buffer trindle-result-buffer)
          (trindle-message "Trindle Update END"))))))

(defun trindle:clear ()
  (interactive)
  (let ((remove-packages (remove-if
                          '(lambda (package) (member package (trindle-registered-pkg-list)))
                          (trindle-installed-pkg-list))))
    (and remove-packages (trindle:remove remove-packages))))

(defun trindle:remove (&optional remove-pkg-list)
  (interactive)
  (dolist (remove-package (or remove-pkg-list (trindle-installed-pkg-list)))
    (let ((remove-dir (trindle-get-install-dir remove-package)))
      (if (file-directory-p remove-dir)
          (progn (dired-delete-file remove-dir 'always)
                 (trindle-delete-load-path remove-dir)
                 (trindle-message "\"%s\" was Deleted." remove-dir))
        (trindle-message "Could not find package \"%s\"" remove-package)))))

(defun trindle:install! ()
  (interactive)
  (trindle:install)
  (trindle:clear))

(defun trindle-github-clone (package)
  (let* ((name (plist-get package :name))
         (url  (format trindle-github-base-url name)))
    (trindle-git-clone (append package (list :url url)))))

(defun trindle-git-clone (package)
  (lexical-let* ((url (plist-get package :url))
                 (branch (or (plist-get package :branch) "master"))
                 (package-name (trindle-get-package-name url))
                 (install-dir  (trindle-get-install-dir package-name)))
    (unless (file-directory-p install-dir)
      (deferred:$
        (deferred:process "git" "clone" "-b" branch url (expand-file-name install-dir))
        (deferred:nextc it
          (lambda ()
            (if trindle-byte-compile
                (trindle-byte-compile-exec install-dir))))
        (deferred:nextc it
          (lambda ()
            (trindle-write-load-path install-dir)
            (trindle-message "[OK] Package %s:%s Installed." package-name branch)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s:%s Install Failure." package-name branch)))))))

(defun trindle-svn-checkout (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (plist-get package :name))
                 (install-dir  (trindle-get-install-dir package-name)))
    (unless (file-directory-p install-dir)
      (deferred:$
        (deferred:process "svn" "checkout" url (expand-file-name install-dir))
        (deferred:nextc it
          (lambda ()
            (if trindle-byte-compile
                (trindle-byte-compile-exec install-dir))))
        (deferred:nextc it
          (lambda ()
            (trindle-write-load-path install-dir)
            (trindle-message "[OK] Package %s Installed." package-name)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-emacswiki-install (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-install (append package (list :url url)))))

(defun trindle-http-install (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (unless (file-directory-p install-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (forward-char)
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (if (string-match "\\^<!DOCTYPE" (buffer-substring-no-properties
                                                (point-at-bol) (point-at-eol)))
                  (trindle-message "[NG] Package %s Install Failure." package-name)
                (progn
                  (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                    (make-directory install-dir t)
                    (write-file (trindle-get-install-path package-name))
                    (trindle-write-load-path install-dir)
                    (if trindle-byte-compile
                        (trindle-byte-compile-exec install-dir))
                    (trindle-message "[OK] Package %s Installed." package-name))))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-github-pull (package)
  (let* ((repository-name (plist-get package :name))
         (url (format trindle-github-base-url repository-name)))
    (trindle-git-pull (append package (list :url url)))))

(defun trindle-git-pull (package)
  (lexical-let* ((url (plist-get package :url))
                 (branch (or (plist-get package :branch) "master"))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (if (file-directory-p install-dir)
        (deferred:$
          (setq default-directory install-dir)
          (deferred:process "git" "pull" "origin" branch)
          (deferred:nextc it
            (lambda ()
              (if trindle-byte-compile (trindle-byte-compile-exec install-dir))))
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s:%s Updated." package-name branch)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s:%s Updated Failuer." package-name branch)))))))

(defun trindle-svn-update (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (plist-get package :name))
                 (install-dir (trindle-get-install-dir package-name)))
    (if (file-directory-p install-dir)
        (deferred:$
          (setq default-directory install-dir)
          (deferred:process "svn" "update")
          (deferred:nextc it
            (lambda ()
              (if trindle-byte-compile
                  (trindle-byte-compile-exec install-dir))))
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s Updated." package-name)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(defun trindle-emacswiki-update (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-update (append package (list :url url)))))

(defun trindle-http-update (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (when (file-directory-p install-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (forward-char)
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (if (string-match "\\^<!DOCTYPE" (buffer-substring-no-properties
                                                (point-at-bol) (point-at-eol)))
                  (trindle-message "[NG] Package %s Update Failure." package-name)
                (progn
                  (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                    (delete-file (trindle-get-install-path package-name))
                    (write-file (trindle-get-install-path package-name))
                    (if trindle-byte-compile
                        (trindle-byte-compile-exec install-dir))
                    (trindle-message "[OK] Package %s Updated." package-name))))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(defun trindle-byte-compile-exec (install-path)
  (deferred:$
    (apply 'deferred:process-shell
           (append (list "emacs" "-L" install-path "-batch" "-f" "batch-byte-compile")
                   (remove-if-not (lambda (path)
                                    (string-match "\\.el$" (file-name-nondirectory path)))
                                  (trindle-get-recuresive-path-list install-path))))
    (deferred:error it
      (lambda (err) (message "%s" err)))))

(provide 'trindle)
;;; trindle.el ends here
