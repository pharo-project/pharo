accessing-properties
propertyAt: propName
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]