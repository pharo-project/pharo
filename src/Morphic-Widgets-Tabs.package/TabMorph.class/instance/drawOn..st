drawing
drawOn: aCanvas
	| borderVertices vertices |
	
	borderVertices := self buildBorderVertices.
	vertices := self buildVerticesFrom: borderVertices.
	
	aCanvas
		drawPolygon: vertices
		color: self backgroundColor
		borderWidth: 0
		borderColor: Color transparent.
		
	borderVertices overlappingPairsDo: [ :start :end | 
		aCanvas
			line: start
			to: end
			width: 1
			color: self borderColor ]