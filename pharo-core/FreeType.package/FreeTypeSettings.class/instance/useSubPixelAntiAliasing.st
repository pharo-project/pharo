accessing
useSubPixelAntiAliasing
	^ self forceNonSubPixelCount == 0 and: [ self bitBltSubPixelAvailable ]