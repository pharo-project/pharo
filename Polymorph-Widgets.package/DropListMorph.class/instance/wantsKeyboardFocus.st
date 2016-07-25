testing
wantsKeyboardFocus
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally)."

	^super wantsKeyboardFocus and: [
		self allowKeyboardFocus]