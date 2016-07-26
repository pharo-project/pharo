public
entryAt: aResolvable
	| path |
	path := self resolve: aResolvable.
	^ store
		nodeAt: path
		ifPresent: [ :node | store entryFromNode: node fileSystem: self path: path ]
		ifAbsent: [ store signalFileDoesNotExist: path ]