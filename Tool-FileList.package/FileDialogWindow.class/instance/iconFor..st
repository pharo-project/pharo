widget API
iconFor: anEntry
	"Answer the icon to use for the directory entry."

	^ anEntry isDirectory
		ifTrue: [ Smalltalk ui icons iconNamed: #smallOpenIcon ]
		ifFalse: [ (self isImageFile: anEntry basename)
				ifTrue: [ Smalltalk ui icons iconNamed: #smallPaintIcon ]
				ifFalse: [ Smalltalk ui icons iconNamed: #smallLeftFlushIcon ] ]