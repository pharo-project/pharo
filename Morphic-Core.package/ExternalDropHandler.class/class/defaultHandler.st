accessing
defaultHandler
	DefaultHandler ifNil: [DefaultHandler := ExternalDropHandler type: nil extension: nil action: [:dropStream | dropStream edit]].
	^DefaultHandler