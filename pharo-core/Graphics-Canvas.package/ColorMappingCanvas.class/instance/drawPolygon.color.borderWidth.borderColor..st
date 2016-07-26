drawing-polygons
drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc
	"Draw the given polygon."
	^myCanvas
		drawPolygon: vertices
		color: aColor
		borderWidth: bw
		borderColor: (self mapColor: bc)