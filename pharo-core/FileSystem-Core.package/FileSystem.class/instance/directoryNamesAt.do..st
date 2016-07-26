public-enumerating
directoryNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]