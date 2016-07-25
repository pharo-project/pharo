visiting
visitKeyUpEvent: anEvent
	| keyEvent |
	keyEvent := KeyboardEvent new
		setType: #keyUp
		buttons: (self convertModifiers: anEvent modifiers)
		position: anEvent position
		keyValue: (self mapSymbolToKeyValue: anEvent symbol)
		charCode: (self mapSymbolToKeyValue: anEvent symbol)
		hand:  self activeHand
		stamp: Time millisecondClockValue.
	keyEvent scanCode: anEvent scanCode.
	^ keyEvent