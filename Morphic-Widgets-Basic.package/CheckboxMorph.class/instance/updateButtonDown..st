updating
updateButtonDown: evt
	"Check for keyboard focus."

	self wantsKeyboardFocusOnMouseDown ifTrue: [
		self takeKeyboardFocus].
	self updateButton: evt