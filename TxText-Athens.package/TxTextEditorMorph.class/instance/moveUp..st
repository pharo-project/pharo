navigation
moveUp: extendingSelection
	"move up visually"
	| pos visualPos point |
	pos := controller cursorPosition.
	visualPos := layoutViewMorph layoutPositionAt:  pos ifOutside: [
		"outside of layout, lets move down via text"
		controller moveUp.

		^ self updateCursor: true.
		].
	"we are subtracting small constant to point just outside the bounds at the top of current line"

	point := visualPos topPointInView - (0@0.0001).
	layoutViewMorph ensurePointWithinLayout: point.

	pos := layoutViewMorph textPositionAt: point.

	extendingSelection 
		ifTrue: [ controller moveSelectionEndTo:pos ] 
		ifFalse: [ controller moveCursorTo: pos ].
	self updateCursor: true.