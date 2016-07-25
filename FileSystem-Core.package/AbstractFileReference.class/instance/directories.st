enumerating
directories
	"Return all the directories (by opposition to files) contained in the receiver"

	| reference |
	reference := self resolve.
	^ (reference fileSystem directoryNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]