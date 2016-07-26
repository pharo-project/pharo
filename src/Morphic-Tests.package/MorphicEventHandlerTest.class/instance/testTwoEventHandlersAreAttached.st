tests
testTwoEventHandlersAreAttached

	| keyboardEvent first second |
	keyboardEvent := KeyboardEvent new 
							setType: #keystroke
							buttons: 2
							position: nil
							keyValue: 65
							charCode: 65
							hand: nil
							stamp: nil.

	first := false.
	second := false.
	morph eventHandler on: #keyStroke send: #value to: [ first := true ].
	morph eventHandler on: #keyStroke send: #value to: [ second := true ].
	morph handleKeystroke: keyboardEvent.
	
	self assert: first.
	self assert: second.