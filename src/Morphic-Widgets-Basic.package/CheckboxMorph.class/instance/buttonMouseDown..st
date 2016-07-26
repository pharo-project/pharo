event handling
buttonMouseDown: evt
	"Sent from the checkbox button to handle focus."

	self wantsKeyboardFocusOnMouseDown ifTrue: [
		self takeKeyboardFocus]