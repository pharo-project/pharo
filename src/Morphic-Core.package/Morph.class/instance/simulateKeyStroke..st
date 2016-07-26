event testing
simulateKeyStroke: aCharacter
	| event |

	event := KeyboardEvent new 
				setType: #keystroke 
				buttons: 0 
				position: 0@0 
				keyValue: aCharacter charCode 
				charCode: aCharacter charCode
				hand: ActiveHand 
				stamp: 0.
	self keyStroke: event