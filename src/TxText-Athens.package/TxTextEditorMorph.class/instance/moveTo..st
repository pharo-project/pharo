navigation
moveTo: aPoint
	"when user clicks on text, move a text cursor close to hand position"
	| textPosition |
	textPosition := layoutViewMorph textPositionAt: aPoint.
	controller moveCursorTo: textPosition.
	self updateCursor: true.
	