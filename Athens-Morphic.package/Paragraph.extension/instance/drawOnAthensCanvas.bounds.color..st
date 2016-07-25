*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas bounds: aRectangle color: aColor
	self displayCaretAndSelectionOnAthensCanvas: anAthensCanvas.
	self displayExtraSelectionOnAthensCanvas: anAthensCanvas.
	anAthensCanvas setPaint: aColor.
	self displayTextOnAthensCanvas: anAthensCanvas in: aRectangle.
