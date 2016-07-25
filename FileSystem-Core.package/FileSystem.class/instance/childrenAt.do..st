public-enumerating
childrenAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]