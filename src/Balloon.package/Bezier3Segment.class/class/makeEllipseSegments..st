utilities
makeEllipseSegments: aRectangle
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates four bezier segments (one for each quadrant) approximating the oval."
	"EXAMPLE: 
	This example draws an oval with a red border and overlays the approximating bezier segments on top of the oval (drawn in black), thus giving an impression of how closely the bezier resembles the oval. Change the rectangle to see how accurate the approximation is for various radii of the oval.
		| rect |
		rect := 100@100 extent: 500@200.
		Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
		(Bezier3Segment makeEllipseSegments: rect) do:[:seg|
			seg lineSegmentsDo:[:last :next|
				Display getCanvas line: last to: next width: 1 color: Color black]].
	"
	"EXAMPLE: 
		| minRadius maxRadius |
		maxRadius := 300.
		minRadius := 20.
		maxRadius to: minRadius by: -10 do:[:rad|
			| rect |
			rect := 400@400 - rad corner: 400@400 + rad.
			Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
			(Bezier3Segment makeEllipseSegments: rect) do:[:seg|
				seg lineSegmentsDo:[:last :next|
					Display getCanvas line: last to: next width: 1 color: Color black]]].
	"
	^self makeEllipseSegments: aRectangle count: 4