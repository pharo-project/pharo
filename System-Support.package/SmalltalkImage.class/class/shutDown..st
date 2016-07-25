initialization
shutDown: isImageClosing

	isImageClosing ifTrue: [ SourceFiles close ].