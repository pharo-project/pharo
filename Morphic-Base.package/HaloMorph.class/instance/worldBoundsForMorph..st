geometry
worldBoundsForMorph: aMorph
	"Answer the rectangle to be used as the inner dimension of aMorph halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	| r |
	r := (self haloEnclosesFullBounds)
		ifFalse: [ aMorph boundsIn: nil ]
		ifTrue: [ aMorph fullBoundsInWorld ].
	self showBoundsInHalo ifTrue: [ ^r expandBy: 2 ].
	^r