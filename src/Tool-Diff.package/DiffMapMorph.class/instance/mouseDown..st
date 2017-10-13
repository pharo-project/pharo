event handling
mouseDown: event
	"Trigger a 0..1 range based on location."
	
	|b|
	b := self innerBounds insetBy: 2.
	self
		triggerEvent: #mapClicked
		with: (((event cursorPoint y asFloat - b top / b height) 
				min: 1) max: 0)