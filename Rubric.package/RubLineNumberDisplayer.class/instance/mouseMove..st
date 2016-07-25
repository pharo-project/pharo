event handling
mouseMove: anEvent
	| lineIndex line |
	lineIndex := self lineIndexForPoint: anEvent position.
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft.
	self textArea mouseMove: anEvent
