context
previousPC: aContext
	^ (aContext method pcPreviousTo: aContext pc)
		ifNil: [ aContext pc ]