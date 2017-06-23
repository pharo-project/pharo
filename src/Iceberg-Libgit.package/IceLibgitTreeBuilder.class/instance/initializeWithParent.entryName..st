initializing
initializeWithParent: anIceTreeBuilder entryName: aString
	| treeEntry |
	parent := anIceTreeBuilder.
	entryName := aString.

	treeEntry := parent builder getEntryNamed: entryName.
	treeEntry isReady "i.e. the entry alredy exists in parent tree"
		ifTrue: [ initialTree := treeEntry tree ]
		ifFalse: [ 
			"The entry didn't exist so this is a new entry"
			hasChanges := true ]
