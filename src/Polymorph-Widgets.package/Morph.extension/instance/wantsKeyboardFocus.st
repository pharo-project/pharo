*Polymorph-Widgets
wantsKeyboardFocus
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally)."

	^self takesKeyboardFocus and: [
		self visible and: [self enabled]]