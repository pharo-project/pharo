make arrows
makeAssignArrow
	"Replace the underline character with an arrow for this font"
	| arrowForm arrowCanvas arrowY arrowLeft arrowRight arrowHeadLength |
	arrowForm := (self characterFormAt: $_) copy.
	arrowCanvas := arrowForm getCanvas.
	arrowCanvas fillColor: Color white.
	arrowY := arrowForm height // 2.
	arrowLeft := 0.
	arrowRight := arrowForm width - 2.
	arrowHeadLength := (arrowRight - arrowLeft) * 2 // 5.
	"Draw the lines"
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: arrowRight @ arrowY
		color: Color black.
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: (arrowLeft + arrowHeadLength) @ (arrowY - arrowHeadLength)
		color: Color black.
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: (arrowLeft + arrowHeadLength) @ (arrowY + arrowHeadLength)
		color: Color black.

	"Replace the glyph"
	self 
		characterFormAt: $_
		put: arrowForm