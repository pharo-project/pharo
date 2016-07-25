accessing
extent: aPoint
	"Since the day objects cache their bounding box, we have to recompute them if the receiver resizes."

	| result |
	result := super extent: aPoint.
	date notNil
		ifTrue: [self computeDays].
	^result