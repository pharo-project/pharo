event handling
click: event
	"check for right click (menu)"

	(self isYellowButtonReallyPressed: event)
		ifTrue: [ self showMenuForPosition: event cursorPoint ]