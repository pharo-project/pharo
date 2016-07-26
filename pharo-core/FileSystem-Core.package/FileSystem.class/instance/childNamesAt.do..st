public-enumerating
childNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]