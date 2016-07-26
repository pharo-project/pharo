callbacks
keyStroke: anEvent
	| isHandledByModel |
	isHandledByModel := self editView model keystroke: anEvent from: self editView.
	isHandledByModel ifFalse: [ super keyStroke: anEvent ]