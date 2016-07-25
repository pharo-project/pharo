menu messages
internalCallToImplementorsOf: aSelector
	"Open an implementors browser on the given selector"
	
	 (model systemNavigation allImplementorsOf: aSelector) isEmpty
				ifFalse: [ model systemNavigation browseAllImplementorsOf: aSelector]
			
	