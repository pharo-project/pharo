showing
visit: anEntry
	anEntry isDirectory
		ifTrue: [ visitor visitDirectory: anEntry ]
		ifFalse: [ visitor visitFile: anEntry ]