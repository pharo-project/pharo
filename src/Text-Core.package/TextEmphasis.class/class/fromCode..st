instance creation
fromCode: anIntegerBitMask
	^ self new emphasisCode: (anIntegerBitMask bitAnd: self allBits)