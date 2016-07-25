private-focus
takeKeyboardFocus

	self focusOrder ifNotEmpty:[:focus | ^ focus first takeKeyboardFocus ].
	^ self changed: #takeKeyboardFocus with: #()