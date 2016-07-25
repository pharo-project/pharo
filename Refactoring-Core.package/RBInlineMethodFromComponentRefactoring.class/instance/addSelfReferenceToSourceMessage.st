transforming
addSelfReferenceToSourceMessage
	| newArguments |
	newArguments := sourceMessage arguments asOrderedCollection.
	newArguments addFirst: sourceMessage receiver copy.
	sourceMessage renameSelector: (self addArgumentToSelector: sourceMessage selector) andArguments: newArguments