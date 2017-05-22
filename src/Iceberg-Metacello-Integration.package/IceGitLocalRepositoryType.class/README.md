I resolve git local repositories (repositories already existing on your disk).
A gitlocal repositoy is composed of: 

gitlocal://full/path/to/repository

gitlocal  		- The git local identifier
full/path/...	- This is a path to the place where the sources are. 
	
For example, if you have a  repository in '/dev/voyage', which also has sources in subdirectory 'mc', your full path will be: '/dev/voyage/mc'. A part of my responsibilities is to find the git root  in the path provided.

Example: 
------------
A script to install voyage using this would like more or less like this:

Metacello new
	repository: 'gitlocal://Users/esteban/Dev/Repository/voyage/mc';
	baseline: 'Voyage';
	load: 'mongo tests'.
