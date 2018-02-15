Extension { #name : #HandMorph }

{ #category : #'*SUnit-UI-Tests' }
HandMorph >> simulateKeyStroke: aCharacterOrShortcut [

	| shortcut |
	shortcut := aCharacterOrShortcut asKeyCombination.
	self handleEvent: (KeyboardEvent new
		setType: #keystroke
		buttons: shortcut modifier eventCode
		position: self position 
		keyValue: shortcut key asciiValue
		charCode: shortcut key asciiValue
		hand: self activeHand 
		stamp: DateAndTime now).
]
