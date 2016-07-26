public-enumerating
filesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]