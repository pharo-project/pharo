events
mouseDown: event
	"An attempt to break up the old processRedButton code into three phases"

	"no matter what, if shift is pressed extend the selection"
	"no right/yellowbutton click just reposoition the cursor"
	(self paragraph selectionContainsPoint: event cursorPoint)
		ifTrue: [ self mouseDownOnSelectedText: event ]
		ifFalse: [ self mouseDownOnUnselectedText: event ]