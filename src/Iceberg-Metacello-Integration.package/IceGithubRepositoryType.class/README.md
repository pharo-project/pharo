I resolve Github repositories.
A github repositoy is composed of: 

github://username/repository[:commitish][/subdir]

github  		- The github identifier
username 	- The github user
repository	- The guthub repository
commitish	- an optional commitish (a branch, a tag, a commit id)
subdir		- an optional subdirectory where the packages exist.
		
Example: 
------------
A script to install voyage using this would like more or less like this:

Metacello new
	repository: 'github://pharo-nosql/voyage:master/mc';
	baseline: 'Voyage';
	load: 'mongo tests'.