accessing
forceNonSubPixelDuring: aBlock
	forceNonSubPixelCount ifNil:[forceNonSubPixelCount := 0].
	forceNonSubPixelCount := forceNonSubPixelCount + 1.
	aBlock ensure:[forceNonSubPixelCount := forceNonSubPixelCount - 1]