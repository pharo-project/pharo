adding/removing protocols
addProtocol: anObject
	"Adds a protocol named anObject.
	Protocols are not repeated"
	
	anObject ifNil:[ ^self ].
	protocols add: anObject