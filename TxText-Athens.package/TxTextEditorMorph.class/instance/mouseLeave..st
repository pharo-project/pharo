event handling
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
	super mouseLeave: evt.
	self currentHand showTemporaryCursor: nil
	