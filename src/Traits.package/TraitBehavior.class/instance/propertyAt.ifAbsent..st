accessing-properties
propertyAt: propName ifAbsent: aBlock
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock