utilities
makeEllipseSegments: aRectangle count: segmentCount
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates segmentCount bezier segments (one for each quadrant) approximating the oval."
	| count angle center scale |
	center := aRectangle origin + aRectangle corner * 0.5.
	scale := aRectangle extent * 0.5.
	count := segmentCount max: 2. "need at least two segments"
	angle := 360.0 / count.
	^(1 to: count) collect:[:i| | seg |
		seg := self makeUnitPieSegmentFrom: i-1*angle to: i*angle.
		self controlPoints: (seg controlPoints collect:[:pt| pt * scale + center])
	].