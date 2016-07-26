navigation-selection
selectTo: aPoint 
	"when user clicks on text, shift key select close to hand position"
	| textPosition |
	textPosition := layoutViewMorph textPositionAt: aPoint.
	controller moveSelectionEndTo: textPosition.
	self updateCursor: true.
	