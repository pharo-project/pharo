typing support
keystroke: aKeyboardEvent
	self
		completionAround: [ 
			(self selectorChooserHandlesKeyboard: aKeyboardEvent)
				ifFalse: [ super keystroke: aKeyboardEvent ].
			self selectorChooserKeystroke: aKeyboardEvent ]
		keyStroke: aKeyboardEvent