drawing
drawDropShadowOn: aCanvas 
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	self assert: [vertices notEmpty] description: 'a polygon must have at least one point'.
	closed 
		ifTrue: [aCanvas drawPolygon: self getVertices fillStyle: self shadowColor].