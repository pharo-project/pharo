events
mouseDown: evt
	"Handle a mouse down event."
	"Overridden to not grab on mouse down"
	(stayUp or:[self fullContainsPoint: evt position]) 
		ifFalse:[^self deleteIfPopUp: evt]. "click outside"
	self comeToFront