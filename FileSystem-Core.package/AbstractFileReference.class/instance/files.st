enumerating
files
	"Return all the files (as opposed to folders) contained in the receiver"
	
	| reference |
	reference := self resolve.
	^ (reference fileSystem fileNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]