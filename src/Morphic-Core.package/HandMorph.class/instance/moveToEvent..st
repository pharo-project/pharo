private events
moveToEvent: anEvent
	"Issue a mouse move event to make the receiver appear at the given position"
	self handleEvent: (MouseMoveEvent basicNew
		setType: #mouseMove 
		startPoint: self position 
		endPoint: anEvent position 
		trail: (Array with: self position with: anEvent position)
		buttons: anEvent buttons
		hand: self
		stamp: anEvent timeStamp)