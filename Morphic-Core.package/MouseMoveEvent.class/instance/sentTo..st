dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	type == #mouseMove ifTrue:[^anObject handleMouseMove: self].
	^super sentTo: anObject.
