event handling
takeKeyboardFocus
	"Answer whether the receiver can normally take keyboard focus."
	self takesKeyboardFocus ifFalse: [ ^self ].
	^ super takeKeyboardFocus 