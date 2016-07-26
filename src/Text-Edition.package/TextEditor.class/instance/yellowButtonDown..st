events
yellowButtonDown: anEvent
	"special case for right/yellobutton click.
	if clicking outside the current selection select the word."
	(self paragraph selectionContainsPoint: anEvent cursorPoint)
		ifFalse: [
			self mouseMove: anEvent.
			self selectWord ].