focus handling
wantsKeyboardFocusOnMouseDown
	"Answer whether the receiver would like keyboard focus
	on a mouse down event. use a property here for apps that want to take keyboard
	focus when the button is pressed (so that other morphs can, e.g. accept on focus change)."

	^self wantsKeyboardFocus and: [self valueOfProperty: #wantsKeyboardFocusOnMouseDown ifAbsent: [false]]