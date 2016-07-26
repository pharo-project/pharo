event handling
mouseDown: anEvent
	| lineIndex line |
	(anEvent yellowButtonPressed and: [ anEvent commandKeyPressed not ]) "First check for option (menu) click"
		ifTrue: [ ^ (self yellowButtonActivity: anEvent shiftPressed) ].

	lineIndex := self lineIndexForPoint: anEvent position.
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft.
	self textArea mouseDown: anEvent