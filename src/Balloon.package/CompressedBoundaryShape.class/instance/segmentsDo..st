enumerating
segmentsDo: aBlock
	"Enumerate all segments in the receiver and execute aBlock"
	| p1 p2 p3 |
	1 to: points size by: 3 do:[:i|
		p1 := points at: i.
		p2 := points at: i+1.
		p3 := points at: i+2.
		(p1 = p2 or:[p2 = p3]) ifTrue:[
			aBlock value: (LineSegment from: p1 to: p3).
		] ifFalse:[
			aBlock value: (Bezier2Segment from: p1 via: p2 to: p3).
		].
	].