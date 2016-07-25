event handling
mouseDown: evt
	"Details: If this button is triggered on mouse down or the event is the menu gesture, handle it immediately. Otherwise, make a list of buttons (including the receiver) for mouseMove feedback. This allows a simple radio-button effect among the button submorphs of a given morph."

	self enabled ifFalse: [^self].
	allButtons := nil.
	evt yellowButtonPressed ifTrue: [^ self invokeMenu: evt].
	self wantsKeyboardFocusOnMouseDown
		ifTrue: [self takeKeyboardFocus].
	triggerOnMouseDown
		ifTrue: [ self performAction: evt ]
		ifFalse: [
			allButtons := owner submorphs select: [:m | m class = self class].
			self updateFeedbackForEvt: evt].
