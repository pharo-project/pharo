drawing
computeVertices
	self isCurve
		ifTrue: [ self computeSmoothVertices ]
		ifFalse: [ self computeStraightVertices ]