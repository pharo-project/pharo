navigation
moveDown
	"move down visually"
	| pos visualPos point |

	pos := controller cursorPosition.
	visualPos := layoutViewMorph layoutPositionAt:  pos ifOutside: [
		"outside of layout, lets move down via text"
		controller moveDown.
		^ self updateCursor: true.
		].
	
	"we are adding small constant to point just outside the bounds of current line where cursor is located"
	point := visualPos bottomPoint + (0@0.001).
	layoutViewMorph ensurePointWithinLayout: point.

	".. and if there's something more, let layout give us new position "
	controller moveCursorTo: (layoutViewMorph textPositionAt: point).		
	self updateCursor:true.
	