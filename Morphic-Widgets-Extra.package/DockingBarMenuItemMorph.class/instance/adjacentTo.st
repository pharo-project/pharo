selecting
adjacentTo
	owner isFloating
		ifTrue: [^ {self bounds bottomLeft + (5 @ 5)}].
	owner isAdheringToTop
		ifTrue: [^ {self bounds bottomLeft + (5 @ 5)}].
	owner isAdheringToLeft
		ifTrue: [^ {self bounds topRight + (5 @ 5)}].
	owner isAdheringToBottom
		ifTrue: [^ {self bounds topLeft + (5 @ 5)}].
	owner isAdheringToRight
		ifTrue: [^ {self bounds topLeft + (5 @ -5)}].
	^ {self bounds bottomLeft + (3 @ 5)}