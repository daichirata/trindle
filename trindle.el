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
(require 'deferred-ext)

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

(defvar trindle-smp 1
  "The number of concurrencies is controlled.")

(defvar trindle-packages nil
  "Holding the list of packages.")

(defmacro trindle:packages (&rest packages)
  "Macro for defining a package briefly."
  `(setq trindle-packages ',packages))

(defmacro trindle:add-packages (&rest packages)
  "Macro for defining a package briefly."
  `(dolist (package '(,@packages))
     (add-to-list 'trindle-packages package)))

(defun trindle-get-package-name (url_or_repo)
  "Get package name by URL or repository name."
  (file-name-sans-extension (file-name-nondirectory url_or_repo)))

(defun trindle-get-package-dir (package-name)
  "Get install dir by package name."
  (file-name-as-directory
   (concat (file-name-as-directory trindle-dir) package-name)))

(defun trindle-get-install-path (package-name)
  "Get install file path by package name."
  (let ((install-dir (trindle-get-package-dir package-name)))
    (concat (file-name-as-directory install-dir) (concat package-name ".el"))))

(defun trindle-get-method (type action)
  "The method according to action is returned. "
  (plist-get (plist-get trindle-methods (intern (concat ":" type))) action))

(defun trindle-plist-get (list label default)
  "plist-get which returns a default value when a value is nil."
  (let ((value (plist-get list label)))
    (if (eq value nil) default value)))

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
  (mapcar (lambda (package)
            (let* ((type (plist-get package :type))
                   (method (trindle-get-method type action)))
              `(lambda () (,method ',package))))
          trindle-packages))

(defun trindle-add-load-path-recursively (paths)
  "It adds to a load path recursively."
  (let (path)
    (dolist (path paths)
      (let ((default-directory path))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(defun trindle-message (&rest string)
  "Output to the buffer for `trindle-result-buffer'"
  (with-current-buffer (get-buffer-create trindle-result-buffer)
    (goto-char (point-max))
    (insert (concat (apply 'format string) "\n"))))

(defun trindle:initialize ()
  "The path of a package is added to a load path."
  (interactive)
  (trindle-add-load-path-recursively (list trindle-dir)))

(defun trindle:install ()
  (interactive)
  (trindle-message "Trindle Install START")
  (let ((task (trindle-make-task-list :install))
        (smp (cc:semaphore-create trindle-smp)))
    (loop for tsk in task do
          (cc:semaphore-with smp tsk))
    (cc:semaphore-with smp
      (lambda ()
        (trindle-message "Trindle Install END")))))

(defun trindle:update ()
  (interactive)
  (trindle-message "Trindle Update START")
  (let ((task (trindle-make-task-list :update))
        (smp (cc:semaphore-create trindle-smp)))
    (loop for tsk in task do
          (cc:semaphore-with smp tsk))
    (cc:semaphore-with smp
      (lambda ()
        (trindle-message "Trindle Update END")))))

(defun trindle:clear ()
  (interactive)
  (let ((remove-packages
         (remove-if (lambda (package)
                      (member package (trindle-registered-pkg-list)))
                    (trindle-installed-pkg-list))))
    (and remove-packages (trindle:remove remove-packages))))

(defun trindle:remove (&optional rp-list)
  (interactive)
  (loop for rp in (or rp-list (trindle-installed-pkg-list)) do
        (let ((remove-dir (trindle-get-package-dir rp)))
          (if (file-directory-p remove-dir)
              (progn (dired-delete-file remove-dir 'always)
                     (trindle-message "\"%s\" was Deleted." remove-dir))
            (trindle-message "Could not find package \"%s\"" rp)))))

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
                 (branch       (trindle-plist-get package :branch "master"))
                 (package-name (trindle-get-package-name url))
                 (package-dir  (trindle-get-package-dir package-name))
                 (trindle-dir  (file-name-as-directory trindle-dir)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:trindle:process
          trindle-dir "git" "--no-pager" "clone" "-b" branch url)
        (deferred:trindle:processc it
          package-dir "git"  "--no-pager" "submodule" "update" "--init" "--recursive")
        (deferred:nextc it
          (lambda ()
            (trindle-message "[OK] Package %s:%s Installed." package-name branch)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s:%s Install Failure." package-name branch)))))))

(defun trindle-github-pull (package)
  (let* ((repository-name (plist-get package :name))
         (url (format trindle-github-base-url repository-name)))
    (trindle-git-pull (append package (list :url url)))))

(defun trindle-git-pull (package)
  (lexical-let* ((url (plist-get package :url))
                 (branch       (trindle-plist-get package :branch "master"))
                 (package-name (trindle-get-package-name url))
                 (package-dir  (trindle-get-package-dir package-name)))
    (if (file-directory-p package-dir)
        (deferred:$
          (deferred:trindle:process
            package-dir "git" "--no-pager" "pull")
          (deferred:trindle:processc it
            package-dir "git" "--no-pager" "submodule" "update" "--init" "--recursive")
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s:%s Updated." package-name branch)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s:%s Updated Failuer." package-name branch)))))))

(defun trindle-svn-checkout (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (plist-get package :name))
                 (package-dir  (trindle-get-package-dir package-name))
                 (trindle-dir  (file-name-as-directory trindle-dir)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:trindle:process trindle-dir "svn" "checkout" url package-name)
        (deferred:nextc it
          (lambda ()
            (trindle-message "[OK] Package %s Installed." package-name)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-svn-update (package)
  (lexical-let* ((package-name (plist-get package :name))
                 (package-dir  (trindle-get-package-dir package-name)))
    (if (file-directory-p package-dir)
        (deferred:$
          (deferred:trindle:process package-dir "svn" "update")
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s Updated." package-name)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

(defun trindle-emacswiki-install (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-install (append package (list :url url)))))

(defun trindle-http-install (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-package-dir package-name)))
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
              (if (string-match "^<!DOCTYPE" (buffer-substring-no-properties
                                                (point-at-bol) (point-at-eol)))
                  (trindle-message "[NG] Package %s Install Failure." package-name)
                (progn
                  (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                    (make-directory install-dir t)
                    (write-file (trindle-get-install-path package-name))
                    (trindle-message "[OK] Package %s Installed." package-name))))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Install Failure." package-name)))))))

(defun trindle-emacswiki-update (package)
  (let* ((elisp-name (plist-get package :name))
         (url (format trindle-emacswiki-base-url elisp-name)))
    (trindle-http-update (append package (list :url url)))))

(defun trindle-http-update (package)
  (lexical-let* ((url (plist-get package :url))
                 (package-name (trindle-get-package-name url))
                 (install-dir (trindle-get-package-dir package-name)))
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
              (if (string-match "^<!DOCTYPE" (buffer-substring-no-properties
                                                (point-at-bol) (point-at-eol)))
                  (trindle-message "[NG] Package %s Update Failure." package-name)
                (progn
                  (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                    (delete-file (trindle-get-install-path package-name))
                    (write-file (trindle-get-install-path package-name))
                    (trindle-message "[OK] Package %s Updated." package-name))))
              (kill-buffer))))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Updated Failuer." package-name)))))))

;; (defun trindle-byte-compile-exec (install-path)
;;   (deferred:$
;;     (apply 'deferred:process-shell
;;            (append (list "emacs" "-L" install-path "-batch" "-f" "batch-byte-compile")
;;                    (remove-if-not (lambda (path)
;;                                     (string-match "\\.el$" (file-name-nondirectory path)))
;;                                   (trindle-get-recuresive-path-list install-path))))
;;     (deferred:error it
;;       (lambda (err) (message "%s" err)))))

(provide 'trindle)
;;; trindle.el ends here
