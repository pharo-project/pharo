display
open
	| window |
	window := StandardWindow new model: self.
	self addAllItems: window.
	window setLabel: self defaultWindowLabel.
	window openInWorld.
	self searchingTextArea contentMorph takeKeyboardFocus