properties
propertyAt: propName put: propValue
	^ self ensureProperties
		at: propName
		put: propValue