event handling
handleFocusEvent: anEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."
	
	self processEvent: anEvent. "give submorphs a chance"
	(anEvent isMouse and: [anEvent isMouseDown and: [(self fullContainsPoint: anEvent position) not]])
		ifFalse: [^super handleFocusEvent: anEvent].
	"click outside - pass to event handler"
	self eventHandler
		ifNotNil: [self eventHandler mouseDown: anEvent fromMorph: self]