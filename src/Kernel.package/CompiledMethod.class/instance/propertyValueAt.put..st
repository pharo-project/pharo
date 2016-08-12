accessing-properties - compatibility
propertyValueAt: propName put: propValue
	"use the version without ..Value, this method is retained for compatibility"
	^self propertyAt: propName put: propValue