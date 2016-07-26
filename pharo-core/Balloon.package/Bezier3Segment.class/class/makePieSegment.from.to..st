utilities
makePieSegment: aRectangle from: angle1 to: angle2
	"Create a single pie segment for the oval inscribed in aRectangle between angle1 and angle2. If angle1 is less than angle2 this method creates a CW pie segment, otherwise it creates a CCW pie segment."
	| seg center scale |
	angle1 > angle2 ifTrue:["ccw"
		^(self makePieSegment: aRectangle from: angle2 to: angle1) reversed
	].
	"create a unit circle pie segment from angle1 to angle2"
	seg := self makeUnitPieSegmentFrom: angle1 to: angle2.
	"scale the segment to fit aRectangle"
	center := aRectangle origin + aRectangle corner * 0.5.
	scale := aRectangle extent * 0.5.
	^self controlPoints: (seg controlPoints collect:[:pt| pt * scale + center])