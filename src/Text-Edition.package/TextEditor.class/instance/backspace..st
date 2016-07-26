typing/selecting keys
backspace: aKeyboardEvent
	| result |
	self closeTypeIn.
	result := super backspace: aKeyboardEvent.
	self editingState keystroke: aKeyboardEvent from: morph.
	^ result