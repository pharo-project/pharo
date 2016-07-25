menu messages
implementorsOf: aSelector
	"Open an implementors browser on the given selector"

	(self model systemNavigation allImplementorsOf: aSelector) isEmpty
		ifTrue: [ self internalCallToBrowse: aSelector ]
		ifFalse: [ self model interactionModel systemNavigation browseAllImplementorsOf: aSelector ]