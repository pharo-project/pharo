visiting
visitTextInputEvent: anEvent
	| keyEvent char mods |
	anEvent text ifNil: [ ^ nil ].
	char := anEvent text first.
	char ifNil: [ ^ nil ].
	mods := anEvent modifiers.
	"If a modifier key is pressed the keystroke event is handled by #visitMouseDownEvent:"
	(mods alt or: [ mods ctrl or: [ mods cmd ] ])
		ifTrue: [ ^ nil ].
		
	keyEvent := KeyboardEvent new
		setType: #keystroke
		buttons: (self convertModifiers: anEvent modifiers)
		position: anEvent position
		keyValue: char charCode
		charCode: char charCode
		hand: self activeHand
		stamp: Time millisecondClockValue.
	
	^ keyEvent