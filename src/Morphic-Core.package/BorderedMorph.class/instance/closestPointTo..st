geometry
closestPointTo: aPoint
	"account for round corners. Still has a couple of glitches at upper left and right corners"
	| pt |
	pt := self bounds pointNearestTo: aPoint.
	self wantsRoundedCorners ifFalse: [ ^pt ].
	self bounds corners with: (self bounds insetBy: 6) corners do: [ :out :in |
		(pt - out) abs < (6@6)
			ifTrue: [ ^(in + (Point r: 5.0 degrees: (pt - in) degrees)) asIntegerPoint ].
	].
	^pt.