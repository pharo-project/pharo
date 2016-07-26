tests
testKeyStrokeFromMorph

	| keyboardEvent |
	keyboardEvent := KeyboardEvent new 
							setType: #keystroke
							buttons: 2
							position: nil
							keyValue: nil
							charCode: 65
							hand: nil
							stamp: nil.

	morph eventHandler on: #keyStroke send: #value to: true.
	
	self assert: ((morph handleKeystroke: keyboardEvent) == true)