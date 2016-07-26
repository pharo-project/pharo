drawing-polygons
drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc
	"Fill the given polygon.
	Note: The default implementation does not recognize any enhanced fill styles"
	self drawPolygon: vertices color: aFillStyle asColor borderWidth: bw borderColor: bc