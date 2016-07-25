accessing
classAt: aName ifAbsent: aBlock
	^ (environment at: aName ifAbsent: aBlock)
		ifNil: aBlock