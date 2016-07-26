private
vertProfile
	vertProfile ifNil: [self computeShadow].
	^ vertProfile