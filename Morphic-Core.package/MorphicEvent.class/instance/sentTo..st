dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	^anObject handleUnknownEvent: self