dispatching
sentTo: anObject
	"Dispatch the receiver into anObject"
	type == #mouseOver ifTrue:[^anObject handleMouseOver: self].
	type == #mouseEnter ifTrue:[^anObject handleMouseEnter: self].
	type == #mouseLeave ifTrue:[^anObject handleMouseLeave: self].
	^super sentTo: anObject.