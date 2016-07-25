public-enumerating
entriesAt: aResolvable ifAbsent: absentBlock do: aBlock
	| path entry aFilesystem |
	path := self resolve: aResolvable.
	aFilesystem := self.
	store
		directoryAt: path
		ifAbsent: [ ^ absentBlock value ]
		nodesDo: [ :node | 
			entry := store entryFromNode: node path: path for: aFilesystem.
			aBlock value: entry ]