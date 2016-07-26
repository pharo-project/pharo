navigation
movePageUp
	"move up visually"
	| pos visualPos point |


	pos := controller cursorPosition.
	visualPos := layoutViewMorph layoutPositionAt:  pos ifOutside: [
		layoutViewMorph scrollIntoView: pos.
		visualPos := layoutViewMorph layoutPositionAt:  pos ifOutside: [ self error: 'that''s not damn possible' ].
		].
	
	
	"we are adding small constant to point just outside the bounds of current line where cursor is located"
	point := visualPos topPointInView.
	layoutViewMorph scrollBy: 0@visualPos layout view height negated.
 
 	controller moveCursorTo: (layoutViewMorph textPositionAt: point).		
	self updateCursor:true.
	