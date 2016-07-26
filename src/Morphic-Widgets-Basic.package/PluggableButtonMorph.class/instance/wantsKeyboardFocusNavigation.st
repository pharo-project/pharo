keyboard
wantsKeyboardFocusNavigation
	"Answer whether the receiver would like keyboard focus
	when navigated to by keyboard."

	^super wantsKeyboardFocusNavigation and: [
		self valueOfProperty: #wantsKeyboardFocusNavigation ifAbsent: [true]]