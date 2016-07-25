geometry
intersectionWithLineSegmentFromCenterTo: aPoint 
	| dx aSquared bSquared m mSquared xSquared x y dy |
	(self containsPoint: aPoint)
		ifTrue: [ ^aPoint ].
	dx := aPoint x - self center x.
	dy := aPoint y - self center y.
	dx = 0
		ifTrue: [ ^self bounds pointNearestTo: aPoint ].
	m := dy / dx.
	mSquared := m squared.
	aSquared := (self bounds width / 2) squared.
	bSquared := (self bounds height / 2) squared.
	xSquared := 1 / ((1 / aSquared) + (mSquared / bSquared)).
	x := xSquared sqrt.
	dx < 0 ifTrue: [ x := x negated ].
	y := m * x.
	^ self center + (x @ y) asIntegerPoint.
