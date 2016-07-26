make arrows
makeReturnArrow
	"Replace the caret character with an arrow"
	| arrowForm arrowCanvas arrowHeadLength arrowX arrowTop arrowBottom |
	arrowForm := (self characterFormAt: $^) copy.
	arrowCanvas := arrowForm getCanvas.
	arrowCanvas fillColor: Color white.
	arrowHeadLength := (arrowForm width - 2) // 2.
	arrowX := arrowHeadLength max: arrowForm width // 2.
	arrowTop := arrowForm height // 4.
	arrowBottom := arrowTop + (arrowForm width * 4 // 5).
	arrowBottom := (arrowBottom min: arrowForm height) max: arrowForm height * 2 // 3.

	"Draw the lines"
	arrowCanvas 
		line: arrowX @ arrowTop
		to: arrowX @ arrowBottom
		color: Color black.
	arrowCanvas 
		line: arrowX @ arrowTop
		to: (arrowX - arrowHeadLength) @ (arrowTop + arrowHeadLength)
		color: Color black.
	arrowCanvas 
		line: arrowX @ arrowTop
		to: (arrowX + arrowHeadLength) @ (arrowTop + arrowHeadLength)
		color: Color black.

	"Replace the glyph"
	self 
		characterFormAt: $^
		put: arrowForm