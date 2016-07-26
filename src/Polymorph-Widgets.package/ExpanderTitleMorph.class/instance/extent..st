geometry
extent: aPoint 
	"Set the receiver's extent to value provided.
	Update the gradient fills."

	|answer|
	aPoint = self extent ifTrue: [^super extent: aPoint].
	answer := super extent: aPoint.
	self fillStyle isOrientedFill
		ifTrue: [self fillStyle: self normalFillStyle].
	^answer