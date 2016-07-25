drawing
drawOn: aCanvas
	"Draw triangles at the edges."
	
	|r|
	r := self horizontal
		ifTrue: [self bounds insetBy: (2@1 corner: 2@1)]
		ifFalse: [self bounds insetBy: (1@2 corner: 1@2)].
	aCanvas
		drawPolygon: (self leftOrTopVertices: self bounds)
		fillStyle: self borderColor;
		drawPolygon: (self leftOrTopVertices: r)
		fillStyle: self fillStyle;
		drawPolygon: (self rightOrBottomVertices: self bounds)
		fillStyle: self borderColor;
		drawPolygon: (self rightOrBottomVertices: r)
		fillStyle: self fillStyle