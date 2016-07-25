utilities
makePieSegments: aRectangle from: angle1 to: angle2
	"Create a series of cubic bezier segments for the oval inscribed in aRectangle between angle1 and angle2. The segments are oriented clockwise, to get counter-clockwise segments simply switch angle1 and angle2."
	angle2 < angle1 ifTrue:[
		"ccw segments"
		^(self makePieSegments: aRectangle from: angle2 to: angle1) 
			reversed collect:[:seg| seg reversed]
	].
	"Split up segments if larger than 120 degrees"
	angle2 - angle1 > 120 ifTrue:["subdivide"
		| midAngle |
		midAngle := angle1 + angle2 * 0.5.
		^(self makePieSegments: aRectangle from: angle1 to: midAngle),
			(self makePieSegments: aRectangle from: midAngle to: angle2).
	].
	"Create actual pie segment"
	^self makePieSegment: aRectangle from: angle1 to: angle2
