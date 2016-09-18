accessing
icon
	"Answer a form to be used as icon"

	"^ item isRemoteDirectory
		ifTrue: [ Smalltalk ui icons smallRemoteOpenIcon]
		ifFalse: [Smalltalk ui icons smallOpenIcon]"

	^ self iconNamed: #smallOpenIcon