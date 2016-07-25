*Polymorph-Widgets
wantsKeyboardFocus
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally). Even if disabled
	we allow for text morphs since can potentially copy text."

	^self takesKeyboardFocus and: [
		self visible and: [
			self enabled or: [self valueOfProperty: #wantsKeyboardFocusWhenDisabled ifAbsent: [true]]]]
