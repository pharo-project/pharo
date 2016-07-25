dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	
	type == #mouseWheel ifTrue:[^anObject handleMouseWheel: self].
	^super sentTo: anObject.
