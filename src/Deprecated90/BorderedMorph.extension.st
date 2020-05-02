Extension { #name : #BorderedMorph }

{ #category : #'*Deprecated90' }
BorderedMorph >> closestPointTo: aPoint [
	"account for round corners. Still has a couple of glitches at upper left and right corners"
	| pt |
	self deprecated: 'No senders in default image, removed in issue #6282.' on: '2020-05-02' in: #Pharo9.  
	pt := self bounds pointNearestTo: aPoint.
	self wantsRoundedCorners ifFalse: [ ^pt ].
	self bounds corners with: (self bounds insetBy: 6) corners do: [ :out :in |
		(pt - out) abs < (6@6)
			ifTrue: [ ^(in + (Point r: 5.0 degrees: (pt - in) degrees)) asIntegerPoint ].
	].
	^pt.
]
