# Notice

    I'm not good at English.
    So please correct my English.

# Trindle.el

## Commentary
'trindle.el' referred to "bundler" of programming language ruby, and created it.
 Unlike package management, only the function of simple management is offered.
 These manage that in which I have installed what from init.el(or any config file).

## Installation

    $git clone--recursive https://github.com/daic-h/trindle.git

## How to use basic

to require the trindle.

    (require 'trindle)

define the package.

    (trindle: packages
       (:type "github":name "daic-h/trindle"))

You can install, update, and remove by defining the package.

    (trindle: install)
    (trindle: install!); delete a file that is not defined in the package, perform the install.
    (trindle: update)
    (trindle: update-all)
    (trindle: remove)

Also, these commands can be called interactive.

call (trindle:initialize), the package file is loaded if defined has been installed.

## Customization

Definition of package

- Common
 - :init-submodule init a submodule to if you want to clone, _default t_
 - :byte-compile   byte-compiled if you want to clone,       _default t_
 - :load-package   to load when call the trindle:itialize,   _default t_

These default values can be changed by trindle:configure

- emacswiki
 - :type "emacswiki"
 - :name - the name of the package emacswiki, _required_

- github
 - :type "github"
 - :name -"username/repository", _required_
 - :branch - branch name when you clone, _default "master"_

- git
 - :type "git"
 - :url - url of the repository, _required_
 - :name - the name of the package, _required_
 - :branch - branch name when you clone, _default "master"_

- svn
 - :type "svn"
 - :url - url of the repository, _required_
 - :name - the name of the package, _required_

- http
 - :type "http"
 - :url - url of the elis-file, _required_
 - :name - the name of the package, _required_

- http-tar
 - :type "http-tar"
 - :url - url of the elis-file, _required_
 - :name - the name of the package, _required_

Argument by (trindle:configure)

 - :dir - directory where the package is installed, _default "~/.emacs.d/trindle"_
 - :smp - number to be executed at the same time in the install and update, _default 1_
 - :init-submodule - it will be the initial value of the package, _default t_
 - :byte-compile   - it will be the initial value of the package, _default t_
 - :load-package   - it will be the initial value of the package, _default t_

## Sample Setting

    (require 'trindle)

    (trindle:packages
     (:type "github" :name "daic-h/initialize")
     (:type "github" :name "emacsmirror/shell-pop")
     (:type "emacswiki" :name "auto-async-byte-compile")
     (:type "http" :name "ruby-mode" :url "http://bugs.ruby-lang.org/projects/ruby-trunk/repository/raw/misc/ruby-mode.el"))

    (trindle:configure :dir "~/.emacs.d/elisp/bundle/" :smp 3)
    (trindle:initialize)

## Credits

 kiwanami/emacs-deffered plays an important feature of this library.