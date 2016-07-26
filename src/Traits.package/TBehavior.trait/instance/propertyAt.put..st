accessing-properties
propertyAt: propName put: propValue
	^ self ensureProperties
		at: propName
		ifAbsentPut: propValue