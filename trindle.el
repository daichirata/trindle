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
;;     - Package is summarized in a group.
;;     - Byte compile processing after installation is added.
;;
;;; Samples: It is temporary now :p
;;
;;   (require 'trindle)
;;   (setq trindle-dir "~/.emacs.d/elisp/trindle")
;;
;;   (trindle:packages
;;     (:type "github" :name "ejmr/php-mode")
;;     (:type "github" :name "defunkt/coffee-mode")
;;     (:type "github" :name "daic-h/trindle")
;;     (:type "emacswiki" :name "auto-complete")
;;     (:type "emacswiki" :name "ruby-block")
;;     (:type "http"      :name "http://www.emacswiki.org/emacs/download/anything.el"))
;;
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

(defcustom trindle-init-file
  (concat (file-name-as-directory trindle-dir) ".trindle.el")
  "The configuration file for adding the load path"
  :type 'file
  :group 'trindle)

(defcustom trindle-load-packages
  t
  "Reads the init file on startup"
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

(defvar trindle-packages nil
  "Holding the list of packages.")

(defmacro trindle:packages (&rest packages)
  "Macro for defining a package briefly."
  `(dolist (package '(,@packages))
     (add-to-list 'trindle-packages package)))

(defun trindle-init ()
  "Load init file on startup"
  (if (file-exists-p trindle-init-file) (load trindle-init-file)))

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

(defun trindle-installed-pkg-list ()
  "List of the packages installed is returned."
  (remove-if '(lambda (package) (equal 0 (string-match "\\." package)))
             (directory-files trindle-dir)))

(defun trindle-registered-pkg-list ()
  "List of the packages registered is returned."
  (let ((result '()))
    (dolist (package trindle-packages result)
      (let* ((name (plist-get package :name))
             (url  (plist-get package :url))
             (package-name (trindle-get-package-name (or name url))))
        (setq result (append result (list package-name)))))))

(defun trindle-write-load-path (install-path)
  "Writing the load path in the configuration file"
  (add-to-list 'load-path install-path)
  (append-to-file
   (format "(add-to-list 'load-path \"%s\")\n" install-path) nil trindle-init-file)
  ;;(trindle-message "add load path \"%s\"" install-path)
  )

(defun trindle-delete-load-path (install-path)
  "Remove the load path from the configuration file"
  (when (file-exists-p trindle-init-file)
    (with-temp-buffer
      (insert-file-contents-literally trindle-init-file)
      (goto-char (point-min))
      (delete-matching-lines install-path)
      (write-file trindle-init-file)
      (kill-buffer)
      ;;(trindle-message "remove load path \"%s\"" install-path)
      )))

(defun trindle-message (&rest string)
  "Output to the buffer for trindle-result-buffer"
  (with-current-buffer (get-buffer-create trindle-result-buffer)
    (insert (concat (apply 'format string) "\n"))))

(defun trindle:install ()
  (interactive)
  (dolist (package trindle-packages)
    (trindle-delegate "install" package)))

(defun trindle:install! ()
  (interactive)
  (trindle:install)
  (trindle:clear))

(defun trindle:update ()
  (interactive)
  (dolist (package trindle-packages)
    (trindle-delegate "update" package)))

(defun trindle:clear ()
  (interactive)
  (let ((remove-packages
         (remove-if '(lambda (package) (member package (trindle-registered-pkg-list)))
                    (trindle-installed-pkg-list))))
    (and remove-packages (trindle:remove remove-packages))))

(defun trindle:remove (&optional remove-pkg-list)
  (interactive)
  (dolist (remove-package (or remove-pkg-list (trindle-installed-pkg-list)))
    (let ((remove-dir (trindle-get-install-dir remove-package)))
      (if (file-accessible-directory-p remove-dir)
          (progn (dired-delete-file remove-dir 'always)
                 (trindle-delete-load-path remove-dir)
                 (trindle-message "\"%s\" was Deleted." remove-dir))
        (trindle-message "Could not find package \"%s\"" remove-package)))))

(defun trindle-delegate (action package)
  (let ((type (plist-get package :type)))
    (cond ((equal action "install")
           (cond ((equal "http"      type) (trindle-http-install      package))
                 ((equal "emacswiki" type) (trindle-emacswiki-install package))
                 ((equal "git"       type) (trindle-git-clone         package))
                 ((equal "github"    type) (trindle-github-clone      package))
                 ((equal "svn"       type) (trindle-svn-checkout      package))))
          ((equal action "update")
           (cond ((equal "http"      type) (trindle-http-update       package))
                 ((equal "emacswiki" type) (trindle-emacswiki-update  package))
                 ((equal "git"       type) (trindle-git-pull          package))
                 ((equal "github"    type) (trindle-github-pull       package))
                 ((equal "svn"       type) (trindle-svn-update        package)))))))

(defun trindle-github-clone (package)
  (let* ((name (plist-get package :name))
         (url  (format trindle-github-base-url name)))
    (trindle-git-clone (append package (list :url url)))))

(defun trindle-emacswiki-install (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-install (append package (list :url url)))))

(defun trindle-git-clone (package)
  (lexical-let* ((url (plist-get package :url))
                 (branch (or (plist-get package :branch) "master"))
                 (package-name (trindle-get-package-name url))
                 (install-dir  (trindle-get-install-dir package-name)))
    (unless (file-accessible-directory-p install-dir)
      (deferred:$
        (deferred:process "git" "clone" "-b" branch url (expand-file-name install-dir))
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
    (unless (file-accessible-directory-p install-dir)
      (deferred:$
        (deferred:process "svn" "checkout" url (expand-file-name install-dir))
        (deferred:nextc it
          (lambda ()
            (trindle-write-load-path install-dir)
            (trindle-message "[OK] Package %s Installed." package-name)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-http-install (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (unless (file-accessible-directory-p install-dir)
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
                  (make-directory install-dir t)
                  (write-file (trindle-get-install-path package-name))
                  (trindle-message "[OK] Package %s Installed." package-name)
                  (trindle-write-load-path install-dir)))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-github-pull (package)
  (let* ((repository-name (plist-get package :name))
         (url (format trindle-github-base-url repository-name)))
    (trindle-git-pull (append package (list :url url)))))

(defun trindle-emacswiki-update (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-update (append package (list :url url)))))

(defun trindle-git-pull (package)
  (lexical-let* ((url (plist-get package :url))
                 (branch (or (plist-get package :branch) "master"))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (if (file-accessible-directory-p install-dir)
        (deferred:$
          (setq default-directory install-dir)
          (deferred:process "git" "pull" "origin" branch)
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s Updated." package-name)))
          (deferred:error it
            (lambda (err) (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(defun trindle-svn-update (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (plist-get package :name))
                 (install-dir (trindle-get-install-dir package-name)))
    (if (file-accessible-directory-p install-dir)
        (deferred:$
          (setq default-directory install-dir)
          (deferred:process "svn" "update")
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s Updated." package-name)))
          (deferred:error it
            (lambda (err) (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(defun trindle-http-update (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-install-dir package-name)))
    (when (file-accessible-directory-p install-dir)
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
                  (delete-file (trindle-get-install-path package-name))
                  (write-file (trindle-get-install-path package-name))
                  (trindle-message "[OK] Package %s Updated." package-name)))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(provide 'trindle)
;;; trindle.el ends here
