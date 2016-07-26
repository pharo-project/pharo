drawing
computeSelectionShapesFor: aRectangle
	self computeFindReplaceShapesFrom: aRectangle topLeft to: aRectangle bottomRight.
	self computeSecondarySelectionShapesFrom: aRectangle topLeft to: aRectangle bottomRight