drawing
framePolygon: vertices on: aCanvas
	"Frame the given rectangle on aCanvas"
	self framePolyline: vertices on: aCanvas.
	self drawLineFrom: vertices last to: vertices first on: aCanvas.