drawing-polygons
drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc
	"Use a BalloonCanvas"
	self asBalloonCanvas 
		drawPolygon: vertices asArray
		fillStyle: aFillStyle
		borderWidth: bw 
		borderColor: bc