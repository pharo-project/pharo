menu messages
internalCallToImplementorsOf: aSelector
	"Open an implementors browser on the given selector"
	
	 (self model systemNavigation allImplementorsOf: aSelector) isEmpty
				ifFalse: [ self model systemNavigation browseAllImplementorsOf: aSelector]
			
	