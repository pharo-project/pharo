drawing
drawOnAthensCanvas: anAthensCanvas
	super drawOnAthensCanvas: anAthensCanvas.
	self computeSelectionShapesFor: anAthensCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOnAthensCanvas: anAthensCanvas