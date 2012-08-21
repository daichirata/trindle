# trindle.el #

```
I'm not good at English.
So please correct my English.
```

'trindle.el' referred to "bundler" of programming language ruby, and created it.
Unlike package management, only the function of simple management is offered.
These manage that in which I have installed what from init.el.

## Useage: ##

### Configure:

    (require 'trindle)
    (trindle:configure :dir "~/.emacs.d/elisp/trindle" :smp 2 :load-packages t :byte-compile t)
    (trindle:initialize)

    (trindle:packages
     (:type "github" :name "daic-h/trindle")
     (:type "github" :name "kiwanami/emacs-deferred" :byte-compile nil)
     (:type "emacswiki" :name "auto-async-byte-compile" :load-package nil))
     
##### trindle:configure

`trindle:configure`, perform the initial configuration of the trindle.

* `:dir` (default value: "~/.emacs.d/trindle/")

    This field configures the directory of you want save packages.

* `:smp` (default value: 1)
   
    This field configures the number of concurrent updates during the installation.  
    __The same meaning as for the number of simultaneous connections to the network, please pay attention to the setting.__
  
* `:load-packages` (default value: t)

    This field configures whether or not to load the package at the time of `trindle:initialize`.  
    Can be set individually for each package is possible, and this value is applied when the package was omitted.
    
* `:byte-compile` (default value: t)

    This field configures whether or not to automatically compile the package.  
    Can be set individually for each package is possible, and this value is applied when the package was omitted.
         
##### trindle:initialize

If `:load-packages` is t or, `:load-package` of a package is t, a package is added to a `load-path`.   
If you do not plan to add to the load-path,  do not have to run.

##### trindle:packages

`trindle:packages` is Macro for defining a package beriefly.

* `:type`

    currently supported: git github svn emacswiki http http-tar

* `:name`

    Please enter the name of the package.  
    If type github, please enter the username/repository.

* `:url`

    If http, http-tar, the svn, please enter the URL you want to download

* `:load-package` (default value: t)

    This field configures whether or not to load the package at the time of `trindle:initialize`.   
    It overrides the `(trindle:configure :load-packages)`.
    
* `:byte-compile` (default value: t)

    This field configures whether or not to automatically compile the package.  
    It overrides the `(trindle:configure :byte-compile)`.
 
##### trindle:add-packages 

`trindle:packages` will overwrite the value, this is added in the package `add-to-list`.

### Command:

These commands can be run interactive.

##### trindle:install

This command installs the package. For packages that are already installed are not installed, you can safely run multiple times.

##### trindle:install!

This command performs the `trindle:clear` before installation

##### trindle:update

This command updates the specified package.

##### trindle:update-all

This command will update all packages.

##### trindle:clear

This command removes a package that does not exist in the `trndle:packages` has been installed.

##### trindle:remove

This command deletes all packages.
