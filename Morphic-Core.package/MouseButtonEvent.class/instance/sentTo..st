dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	type == #mouseDown ifTrue:[^anObject handleMouseDown: self].
	type == #mouseUp ifTrue:[^anObject handleMouseUp: self].
	^super sentTo: anObject