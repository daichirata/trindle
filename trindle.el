;;; trindle.el --- Simple Emacs LISP management extension

;; Copyright (C) 2012 Daichi Hirata

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Author: Daichi Hirata <daichi.hirat@gmail.com>

;;; Commentary:
;;
;; 'trindle.el' referred to "bundler" of programming language ruby, and created it.
;; Unlike package management, only the function of simple management is offered.
;; These manage that in which I have installed what from init.el(or any config file).

;;; Installation
;;
;; $git clone--recursive https://github.com/daic-h/trindle.git

;;; Useage
;;
;; How to use basic
;;
;;  to require the trindle.
;;   (require 'trindle)
;;
;;  define the package.
;;   (trindle: packages
;;    (:type "github":name "daic-h/trindle"))
;;
;;  You can install, update, and remove by defining the package.
;;
;;   (trindle: install)
;;   (trindle: install!)
;;    - delete a file that is not defined in the package, perform the install.
;;   (trindle: update)
;;   (trindle: update-all)
;;   (trindle: remove)
;;
;;  Also, these commands can be called interactive.
;;  call (trindle:initialize), the package file is loaded if defined has been installed.

;;; Customization
;;
;; Definition of package
;;
;; ・Common
;;  :init-submodule init a submodule to if you want to clone, default t
;;  :byte-compile   byte-compiled if you want to clone,       default t
;;  :load-package   to load when call the trindle:itialize,   default t
;;  These default values can be changed by trindle:configure
;;
;; ・emacswiki
;;  :type "emacswiki"
;;  :name - the name of the package emacswiki, required
;;
;; · github
;;  :type "github"
;;  :name -"username/repository", required
;;  :branch - branch name when you clone, default "master"
;;
;; · git
;;  :type "git"
;;  :url - url of the repository, required
;;  :name - the name of the package, required
;;  :branch - branch name when you clone, default "master"
;;
;; · svn
;;  :type "svn"
;;  :url - url of the repository, required
;;  :name - the name of the package, required
;;
;; ・http
;;  :type "http"
;;  :url - url of the elis-file, required
;;  :name - the name of the package, required
;;
;; ・http-tar
;;  :type "http-tar"
;;  :url - url of the elis-file, required
;;  :name - the name of the package, required
;;
;; Argument by (trindle:configure)
;;  :dir - directory where the package is installed, default "~/.emacs.d/trindle"
;;  :smp - number to be executed at the same time in the install and update, default 1
;;  :init-submodule - it will be the initial value of the package, default t
;;  :byte-compile   - it will be the initial value of the package, default t
;;  :load-package   - it will be the initial value of the package, default t

;;; Sample Setting
;;
;; (require 'trindle)
;;
;; (trindle:packages
;;  (:type "github" :name "daic-h/initialize")
;;  (:type "github" :name "emacsmirror/shell-pop")
;;  (:type "emacswiki" :name "auto-async-byte-compile")
;;  (:type "http" :name "ruby-mode" :url "http://bugs.ruby-lang.org/projects/ruby-trunk/repository/raw/misc/ruby-mode.el"))
;;
;; (trindle:configure :dir "~/.emacs.d/elisp/bundle/" :smp 3)
;; (trindle:initialize)

;;; Credits
;;
;; kiwanami/emacs-deffered plays an important feature of this library.

;;; Code:
(eval-when-compile (require 'cl))
(require 'url)
(require 'dired)
(require 'deferred-ext)

(defgroup trindle nil
  "trindle"
  :group 'trindle)

(defcustom trindle-dir
  "~/.emacs.d/trindle"
  "Path where to install packages."
  :type 'directory
  :group 'trindle)

(defcustom trindle-smp
  1
  "The number of concurrencies is controlled."
  :type 'integer
  :group 'trindle)


(defcustom trindle-load-packages
  t
  "Add a load path of the package at `trindle:initialize'."
  :type 'boolean
  :group 'trindle)

(defcustom trindle-byte-compile
  t
  "Byte compile is performed at the time of install and update."
  :type 'boolean
  :group 'trindle)

(defcustom trindle-init-submodule
  t
  "Init submodule at install and update."
  :type 'boolean
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
    :http-tar  (:install trindle-http-tar-install
                :update  trindle-http-tar-update)
    :svn       (:install trindle-svn-checkout
                :update  trindle-svn-update))
    "The list of methods according to action.")

(defvar trindle-packages nil
  "Holding the list of packages.")

(defvar trindle-log-str nil
  "Holding the string of log message")

(defvar trindle-emacswiki-base-url
  "http://www.emacswiki.org/emacs/download/%s.el"
  "Download URL of emacswiki")

(defvar trindle-github-base-url
  "https://github.com/%s.git"
  "Download URL of Github")

(defmacro trindle:packages (&rest packages)
  "Macro for defining a package briefly."
  `(setq trindle-packages ',packages))

(defmacro trindle:add-packages (&rest packages)
  "Macro for defining a package briefly."
  `(dolist (package '(,@packages))
     (add-to-list 'trindle-packages package)))

(defun* trindle:configure (&key (dir trindle-dir)
                                (smp trindle-smp)
                                (byte-compile trindle-byte-compile)
                                (load-package trindle-load-packages))
  "Initialize the variable dir, smp, byte-compile, load-packages."
  (setq trindle-dir dir
        trindle-smp smp
        trindle-byte-compile byte-compile
        trindle-load-packages load-package))

(defun trindle-get-package-dir (package-name)
  "Get install dir by package name."
  (file-name-as-directory
   (concat (file-name-as-directory trindle-dir)
           package-name)))

(defun trindle-get-package-src-dir (package-name)
  "Get src dir by package name."
  (let* ((path (trindle-get-package-dir package-name))
         (src-path (concat path "src")))
    (when (file-exists-p src-path)
      (file-name-as-directory
       (concat (file-name-as-directory src-path)
               (car (directory-files src-path nil "^[0-9|A-Z|a-z]")))))))

(defun trindle-get-install-path (package-name)
  "Get install file path by package name."
  (let ((install-dir (trindle-get-package-dir package-name)))
    (concat (file-name-as-directory install-dir)
            (concat package-name ".el"))))

(defun trindle-get-method (type action)
  "The method according to action is returned. "
  (plist-get (plist-get trindle-methods
                        (intern (concat ":" type)))
             action))

(defun trindle-plist-get (list label default)
  "When value is not set to list, a default value returns."
  (if (plist-member list label)
      (plist-get list label)
    default))

(defun trindle-get-package-from-name (name)
  "Based on the name, to get the package."
  (loop for package in trindle-packages
        for _name = (file-name-nondirectory (plist-get package :name))
        if (string= name _name) return package))

(defun trindle-installed-pkg-list ()
  "List of the packages installed is returned."
  (directory-files trindle-dir nil "^[0-9|A-Z|a-z]"))

(defun trindle-registered-pkg-list ()
  "List of the packages registered is returned."
  (loop for package in trindle-packages
        for name = (file-name-nondirectory (plist-get package :name))
        collect name))

(defun trindle-load-path-list ()
  "List of packages load path."
  (loop for package in trindle-packages
        for type  = (plist-get package :type)
        for name  = (file-name-nondirectory (plist-get package :name))
        for lpath = (trindle-plist-get package :load-package trindle-load-packages)
        for path  = (if (string= type "http-tar")
                        (trindle-get-package-src-dir name)
                      (trindle-get-package-dir name))
        if (and lpath path) collect path))

(defun trindle-task-list (action)
  "The list of processings for every package is returned."
  (mapcar
   (lambda (package)
     (let* ((type (plist-get package :type))
            (method (trindle-get-method type action)))
       `(lambda () (,method ',package))))
   trindle-packages))

(defun trindle-message (&rest string)
  "Output to the buffer for `trindle-result-buffer'"
  (with-current-buffer (get-buffer-create trindle-result-buffer)
    (goto-char (point-max))
    (insert (concat (apply 'format string) "\n"))))

(defun trindle-log (&rest string)
  "keep a log to `trindle-str'"
  (if (equal '(show) string)
      (trindle-message
       (mapconcat 'identity (reverse trindle-log-str) "\n"))
    (push (apply 'format string) trindle-log-str)))

(defun trindle:initialize ()
  (interactive)
  (loop for path in (trindle-load-path-list)
        for fullpath = (file-name-directory (expand-file-name path))
        do
        (when (file-directory-p fullpath)
          (trindle-log "load-path:%S" fullpath)
          (add-to-list 'load-path fullpath))
        finally (trindle-log 'show)))

(defun trindle:install ()
  (interactive)
  (trindle-message "Trindle Install START")
  (let ((task (trindle-task-list :install))
        (smp (cc:semaphore-create trindle-smp)))
    (loop for tsk in task do
          (cc:semaphore-with smp tsk))
    (cc:semaphore-with smp
      (lambda ()
        (trindle-message "Trindle Install END")))))

(defun trindle:update ()
  (interactive)
  (let* ((name (read-string "package: "))
         (package (trindle-get-package-from-name name)))
    (if package
        (let* ((smp (cc:semaphore-create 1))
               (type (plist-get package :type))
               (method (trindle-get-method type :update)))
          (cc:semaphore-with smp
            (lambda () (trindle-message "Trindle Update START")))
          (cc:semaphore-with smp
            (lambda () (funcall method package)))
          (cc:semaphore-with smp
            (lambda () (trindle-message "Trindle Update END"))))
      (trindle-message "Package:%s is not found." name))))

(defun trindle:update-all ()
  (interactive)
  (trindle-message "Trindle Update START")
  (let ((task (trindle-task-list :update))
        (smp (cc:semaphore-create trindle-smp)))
    (loop for tsk in task do
          (cc:semaphore-with smp tsk))
    (cc:semaphore-with smp
      (lambda ()
        (trindle-message "Trindle Update END")))))

(defun trindle:clear ()
  (interactive)
  (let ((remove-packages
         (loop for pkg in (trindle-installed-pkg-list)
               unless (member pkg (trindle-registered-pkg-list))
               collect pkg)))
    (when remove-packages
      (trindle:remove remove-packages))))

(defun trindle:remove (&optional pkgs)
  (interactive)
  (loop for pkg in (or pkgs (trindle-installed-pkg-list))
        for remove-dir = (trindle-get-package-dir pkg)
        if (file-directory-p remove-dir) do
        (dired-delete-file remove-dir 'always)
        (trindle-message "%S was Deleted." remove-dir)
        else do
        (trindle-message "Could not find package %S" pkg)))

(defun trindle:install! ()
  (interactive)
  (trindle:install)
  (trindle:clear))

(defun trindle-github-clone (package)
  (let* ((name    (plist-get package :name))
         (url     (format trindle-github-base-url name))
         (package (plist-put package :name
                             (file-name-nondirectory name))))
    (trindle-git-clone (append package (list :url url)))))

(defun trindle-github-pull (package)
  (let* ((name    (plist-get package :name))
         (url     (format trindle-github-base-url name))
         (package (plist-put package :name
                             (file-name-nondirectory name))))
    (trindle-git-pull (append package (list :url url)))))

(defun trindle-git-clone (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (branch      (trindle-plist-get package :branch "master"))
                 (package-dir (trindle-get-package-dir name))
                 (trindle-dir (file-name-as-directory trindle-dir))
                 (submodule   (trindle-plist-get package :init-submodule trindle-init-submodule))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:trindle:process
          trindle-dir "git" "--no-pager" "clone" "-b" branch url name)
        (if submodule
            (deferred:trindle:processc it
              package-dir "git"  "--no-pager" "submodule" "update" "--init" "--recursive") it)
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda ()
            (trindle-message "[OK] Package %s:%s Installed." name branch)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s:%s Install Failure.\n %s" name branch err)))))))

(defun trindle-git-pull (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (branch      (trindle-plist-get package :branch "master"))
                 (package-dir (trindle-get-package-dir name))
                 (submodule   (trindle-plist-get package :init-submodule trindle-init-submodule))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (if (file-directory-p package-dir)
        (deferred:$
          (deferred:trindle:process
            package-dir "git" "--no-pager" "pull")
          (if submodule
              (deferred:trindle:processc it
                package-dir "git" "--no-pager" "submodule" "update" "--init" "--recursive") it)
          (if byte-comp
              (deferred:nextc it
                (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s:%s Updated." name branch)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s:%s Updated Failuer.\n %s" name branch err)))))))

(defun trindle-svn-checkout (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (trindle-dir (file-name-as-directory trindle-dir))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:trindle:process trindle-dir "svn" "checkout" url name)
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda ()
            (trindle-message "[OK] Package %s Installed." name)))
        (deferred:error it
          (lambda (err)
            (trindle-message "[NG] Package %s Install Failure.\n %s" name err)))))))

(defun trindle-svn-update (package)
  (lexical-let* ((name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (if (file-directory-p package-dir)
        (deferred:$
          (deferred:trindle:process package-dir "svn" "update")
          (if byte-comp
              (deferred:nextc it
                (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
          (deferred:nextc it
            (lambda ()
              (trindle-message "[OK] Package %s Updated." name)))
          (deferred:error it
            (lambda (err)
              (trindle-message "[NG] Package %s Updated Failuer.\n %s" name err)))))))

(defun trindle-emacswiki-install (package)
  (let* ((name (plist-get package :name))
         (url  (format trindle-emacswiki-base-url name)))
    (trindle-http-install (append package (list :url url)))))

(defun trindle-emacswiki-update (package)
  (let* ((name (plist-get package :name))
         (url  (format trindle-emacswiki-base-url name)))
    (trindle-http-update (append package (list :url url)))))

(defun trindle-http-install (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it #'trindle-delete-header)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                (make-directory package-dir t)
                (write-file (trindle-get-install-path name)) buf))))
        (deferred:nextc it #'deferred:url-delete-buffer)
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda () (trindle-message "[OK] Package %s Installed." name)))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Install Failure.\n %s" name err)))))))

(defun trindle-http-update (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile)))
    (when (file-directory-p package-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it #'trindle-delete-header)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                (delete-file (trindle-get-install-path name))
                (write-file  (trindle-get-install-path name))) buf)))
        (deferred:nextc it #'deferred:url-delete-buffer)
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda () (trindle-message "[OK] Package %s Updated." name)))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Updated Failuer.\n %s" name err)))))))

(defun trindle-http-tar-install (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile))
                 (src (file-name-as-directory (concat package-dir "src")))
                 (tar (file-name-nondirectory url)))
    (unless (file-directory-p package-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it #'trindle-delete-header)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                (make-directory package-dir t)
                (make-directory src t)
                (write-file (concat package-dir tar))) buf)))
        (deferred:nextc it #'deferred:url-delete-buffer)
        (deferred:trindle:processc it package-dir "tar" "zxvf" tar "-C" "src")
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda () (trindle-message "[OK] Package %s Installed." name)))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Install Failure.\n %s" name err)))))))

(defun trindle-http-tar-update (package)
  (lexical-let* ((url         (plist-get package :url))
                 (name        (plist-get package :name))
                 (package-dir (trindle-get-package-dir name))
                 (byte-comp   (trindle-plist-get package :byte-compile trindle-byte-compile))
                 (src (file-name-as-directory (concat package-dir "src")))
                 (tar (file-name-nondirectory url)))
    (when (file-directory-p package-dir)
      (deferred:$
        (deferred:url-retrieve url)
        (deferred:nextc it #'trindle-delete-header)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (let ((byte-compile-warnings nil) emacs-lisp-mode-hook)
                (delete-file (concat package-dir tar))
                (dired-delete-file src 'always)
                (make-directory src t)
                (write-file (concat package-dir tar))) buf)))
        (deferred:nextc it #'deferred:url-delete-buffer)
        (deferred:trindle:processc it package-dir "tar" "zxvf" tar "-C" "src")
        (if byte-comp
            (deferred:nextc it
              (lambda () (trindle-async-exec "trindle-byte-compile" package-dir))) it)
        (deferred:nextc it
          (lambda () (trindle-message "[OK] Package %s Updated." name)))
        (deferred:error it
          (lambda (err) (trindle-message "[NG] Package %s Update Failure.\n %s" name err)))))))

(defun trindle-delete-header (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (goto-char (point-min))
    (when (string-match
           "^<\\!DOCTYPE\\|^<\\?xml"
           (buffer-substring-no-properties
            (point-at-bol) (point-at-eol)))
      (error "Download goes wrong."))) buf)

(defun trindle-async-exec (funcname package-dir)
  (lexical-let ((trindle (symbol-file 'trindle-async-exec)))
    (deferred:trindle:process package-dir
      "emacs" "-batch"
      "--eval" (format "(setq trindle-package-dir %S)" package-dir)
      "--eval" (format "(setq load-path (append '%S load-path)))" load-path)
      "-L" (file-name-directory trindle)
      "-l" (file-name-sans-extension trindle)
      "-f" funcname)))

(defun trindle-byte-compile ()
  (defvar trindle-package-dir "")
  (let* ((dir trindle-package-dir)
         (default-directory dir)
         (files (directory-files dir t "^[0-9|A-Z|a-z|]"))
         byte-compile-warnings emacs-lisp-mode-hook)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))
    (loop for f in files
          if (file-directory-p f) do
          (byte-recompile-directory f 0)
          else do
          (and (string-match "\\.el$" f) (byte-compile-file f)))))

(provide 'trindle)
;;; trindle.el ends here
