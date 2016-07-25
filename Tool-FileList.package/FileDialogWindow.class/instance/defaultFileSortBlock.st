accessing
defaultFileSortBlock
	"Answer the default file stor block"

	^[:entry1 :entry2 |
		entry1 isDirectory = entry2 isDirectory
			ifTrue: [entry1 basename <= entry2 basename]
			ifFalse: [entry1 isDirectory
						ifTrue: [true]
						ifFalse: [entry2 isDirectory
									ifTrue: [false]
									ifFalse: [entry1 basename <= entry2 basename]]]]