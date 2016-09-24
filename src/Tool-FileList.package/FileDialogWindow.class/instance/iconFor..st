widget API
iconFor: anEntry
	"Answer the icon to use for the directory entry."

	^ anEntry isDirectory
		ifTrue: [ self iconNamed: #smallOpenIcon ]
		ifFalse: [ (self isImageFile: anEntry basename)
				ifTrue: [ self iconNamed: #smallPaintIcon ]
				ifFalse: [ self iconNamed: #smallLeftFlushIcon ] ]