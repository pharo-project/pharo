drop shadows
shadowOffset: aPoint
	"Set the current shadow offset"
	(aPoint isNil or:[ aPoint isZero ])
		ifTrue:[ self removeProperty: #shadowOffset ]
		ifFalse:[ self setProperty: #shadowOffset toValue: aPoint ].