A MCFileTreeGitRepository adds git commands via OSProcess when saving a package in a filetree.
Uses gitfiletree: as a protocol (:().

A gitfiletree url with a protocol parameter is the target for a remote. Otherwise the url is considered as a local file reference.

Parameters are:

* dir : the directory inside the repository where the target MC packages are.
* branch : the git branch to fetch.
* protocol : the protocol to use to access the repository from git. Essentially make the difference between ssh (git@hostname:pathToRepo) and others urls (https://hostname/pathToRepo).
* readOnly : is the repository read only? If yes, reduce the history to a minimum, restrict operations on the GUI and append the created repository name.

Once parameters are set, it becomes mandatory to have a protocol and a remote url. Among parameters, protocol is mandatory, all others are optional.
	
Example :

```smalltalk
MCFileTreeGitRepository fromZnUrl: (ZnUrl fromString: 'gitfiletree://github.com/ThierryGoubier/filetree.git?protocol=git&dir=repository&branch=pharo3.0' ).
```

* Creates a MC repository on a git clone of github.com/ThierryGoubier/filetree.git,
* on branch pharo3.0
* stored in a directory named filetree under the Pharo working directory,
* and pointing to the repository/ subdirectory where the filetree packages are kept.
Alternative syntax:
```smalltalk
MCRepository basicFromUrl: 'gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository' asZnUrl
```

gitfiletree core documentation :

gitfiletree maps MC commands onto git commands via OSProcess, and MC metadata out of git commit data. That's all there is to it. A bit of git knowledge can help, but no git command line work is necessary. The inner workings are a bit more complex than that of course, but not by much.
