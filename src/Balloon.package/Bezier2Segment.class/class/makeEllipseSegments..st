utilities
makeEllipseSegments: aRectangle
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates eight bezier segments (two for each quadrant) approximating the oval."
	"EXAMPLE: 
	This example draws an oval with a red border and overlays the approximating bezier segments on top of the oval (drawn in black), thus giving an impression of how closely the bezier resembles the oval. Change the rectangle to see how accurate the approximation is for various radii of the oval.
		| rect |
		rect := 100@100 extent: 1200@500.
		Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
		(Bezier2Segment makeEllipseSegments: rect) do:[:seg|
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
			(Bezier2Segment makeEllipseSegments: rect) do:[:seg|
				seg lineSegmentsDo:[:last :next|
					Display getCanvas line: last to: next width: 1 color: Color black]]].
	"
	| nrm topCenter leftCenter rightCenter bottomCenter dir scale seg1a topRight seg1b seg2a bottomRight seg2b center bottomLeft topLeft seg3a seg3b seg4a seg4b |
	dir := aRectangle width * 0.5.
	nrm := aRectangle height * 0.5.

	"Compute the eight control points on the oval"
	scale := 0.7071067811865475. "45 degreesToRadians cos = 45 degreesToRadians sin = 2 sqrt / 2"
	center := aRectangle origin + aRectangle corner * 0.5.

	topCenter := aRectangle topCenter.
	rightCenter := aRectangle rightCenter.
	leftCenter := aRectangle leftCenter.
	bottomCenter := aRectangle bottomCenter.

	topRight := (center x + (dir * scale)) @ (center y - (nrm * scale)).
	bottomRight := (center x + (dir * scale)) @ (center y + (nrm * scale)).
	bottomLeft := (center x - (dir * scale)) @ (center y + (nrm * scale)).
	topLeft := (center x - (dir * scale)) @ (center y - (nrm * scale)).

	scale := 0.414213562373095. "2 sqrt - 1"

	dir := (dir * scale) @ 0.
	nrm := 0 @ (nrm * scale).
	
	seg1a := self from: topCenter via: topCenter + dir to: topRight.
	seg1b := self from: topRight via: rightCenter - nrm to: rightCenter.

	seg2a := self from: rightCenter via: rightCenter + nrm to: bottomRight.
	seg2b := self from: bottomRight via: bottomCenter + dir to: bottomCenter.

	seg3a := self from: bottomCenter via: bottomCenter - dir to: bottomLeft.
	seg3b := self from: bottomLeft via: leftCenter + nrm to: leftCenter.

	seg4a := self from: leftCenter via: leftCenter - nrm to: topLeft.
	seg4b := self from: topLeft via: topCenter - dir to: topCenter.

	^{seg1a. seg1b. seg2a. seg2b. seg3a. seg3b. seg4a. seg4b}