*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	| borderVertices vertices path |
	borderVertices := self buildBorderVertices.
	vertices := self buildVerticesFrom: borderVertices.
	path := self createClosedPolygonPathFrom: vertices on: anAthensCanvas.
	anAthensCanvas setPaint: self backgroundColor.
	anAthensCanvas drawShape: path.
	path := self createLinesPathFrom: borderVertices on: anAthensCanvas.
	(anAthensCanvas setStrokePaint: self borderColor) width:1.
	anAthensCanvas drawShape: path.
