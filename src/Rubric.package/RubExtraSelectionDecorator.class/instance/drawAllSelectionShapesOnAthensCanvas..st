drawing
drawAllSelectionShapesOnAthensCanvas: anAthensCanvas
	findReplaceShapes ifNotNil: [ findReplaceShapes do: [ :selblock | self drawSelectionShape: selblock onAthensCanvas: anAthensCanvas ] ].
	secondarySelectionShapes ifNotNil: [ secondarySelectionShapes do: [ :selblock | self drawSelectionShape:  selblock onAthensCanvas: anAthensCanvas] ]