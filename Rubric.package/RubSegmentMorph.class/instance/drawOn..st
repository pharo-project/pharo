drawing
drawOn: aCanvas 
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	|  |
	aCanvas drawPolygon: self getVertices fillStyle: self fillStyle.
	self drawBorderOn: aCanvas
