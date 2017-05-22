utilities
directoryFromPath: aString relativeTo: aCommitOrTree
	^ aCommitOrTree 
		entryByPath: aString 
		ifAbsent: [ 
			IceMissingRepositoryEntry new name: aString; owner: aCommitOrTree; signal ]
 
