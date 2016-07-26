event handling
handlesMouseStillDown: anEvent
	"Still down event is used to scroll the selection 
	when the mouse is outside (upon the top of 
	below the bottom)"
	^ (self innerBounds containsPoint: anEvent position) not