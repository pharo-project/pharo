menu messages
implementorsOf: aSelector
	"Open an implementors browser on the given selector"

	(model systemNavigation allImplementorsOf: aSelector) isEmpty
			ifTrue: [self internalCallToBrowse: aSelector ]
			ifFalse: [ model systemNavigation browseAllImplementorsOf: aSelector]