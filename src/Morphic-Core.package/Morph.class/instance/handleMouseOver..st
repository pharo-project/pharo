events-processing
handleMouseOver: anEvent
	"System level event handling."
	anEvent hand mouseFocus == self ifTrue:[
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: anEvent position event: anEvent) ifFalse:[^self]].
	anEvent hand noticeMouseOver: self event: anEvent