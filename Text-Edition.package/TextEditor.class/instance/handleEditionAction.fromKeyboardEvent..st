typing support
handleEditionAction: anAction fromKeyboardEvent: aKeyboardEvent
	(self editingState handlesKeyboard: aKeyboardEvent from: morph)
		ifFalse: [self handleKeystrokeAction: anAction ].
	self editingState keystroke: aKeyboardEvent from: morph.