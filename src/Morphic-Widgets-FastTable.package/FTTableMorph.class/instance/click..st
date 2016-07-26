event handling
click: event
	"check for right click (menu)"

	(event yellowButtonPressed and: [ (self commandOrCrontrolKeyPressed: event) not ])
		ifTrue: [ self showMenuForPosition: event cursorPoint ]