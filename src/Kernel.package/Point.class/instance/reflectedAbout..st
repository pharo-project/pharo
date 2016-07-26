point functions
reflectedAbout: aPoint
	"Answer a new point that is the reflection of the receiver about the given point."

	^ (self - aPoint) negated + aPoint