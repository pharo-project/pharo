mathematical functions
safeArcCos
	"Answer the angle in radians."
	(self between: -1.0 and: 1.0)
		ifTrue: [^ self arcCos]
		ifFalse: [^ self sign arcCos]