typing/selecting keys
argAdvance: aKeyboardEvent
	"Invoked by Ctrl-a.  Useful after Ctrl-q.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the caret after the space.  If none are found, place the
		caret at the end of the text.  Does not affect the undoability of the 
	 	previous command."

	| start |
	self closeTypeIn.
	start := self text findString: ': ' startingAt: self stopIndex.
	start = 0 ifTrue: [ start := self text size + 1].
	self selectAt: start + 2.
	^true