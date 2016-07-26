visiting
visitKeyDownEvent: anEvent
	| keyEvent keyEvent2 mods |

	mods := anEvent modifiers.
	keyEvent := KeyboardEvent new
		setType: #keyDown
		buttons: (self convertModifiers: mods)
		position: anEvent position
		keyValue: (self mapSymbolToKeyValue: anEvent symbol)
		charCode: (self mapSymbolToKeyValue: anEvent symbol)
		hand:  self activeHand
		stamp: Time millisecondClockValue.
	keyEvent scanCode: anEvent scanCode.
	self enqueue: keyEvent.
	
	"This is for ctrl/alt held alone makes morphic think that null character is good text input"
	keyEvent keyValue = 0 ifTrue: [ ^ self ]. 
	
	
	"That's stupid and horrible, but that's the way we doing it for now"
	keyEvent keyValue = 27 "Esc" ifFalse: [
		anEvent character ifNil: [ 
			(mods alt or: [ mods ctrl or: [ mods cmd ] ]) ifFalse: [ 
				^ nil
			]]
	].
		
	"Workaround for SDL<->Morphic: produce keystroke events
	if any modifier key held down (for non-modified keys there are OSTextInputEvent)"
	
	keyEvent2 := KeyboardEvent new
		setType: #keystroke
		buttons: (self convertModifiers: anEvent modifiers)
		position: anEvent position
		keyValue: (self mapSymbolToKeyValue: anEvent symbol)
		charCode: (self mapSymbolToKeyValue: anEvent symbol)
		hand:  morphicWorld activeHand
		stamp: Time millisecondClockValue.
	keyEvent2 scanCode: anEvent scanCode.

	^ keyEvent2