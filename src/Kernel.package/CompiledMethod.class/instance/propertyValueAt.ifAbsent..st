accessing-properties - compatibility
propertyValueAt: propName ifAbsent: aBlock
	"use the version without ..Value, this method is retained for compatibility"
	^self propertyAt: propName ifAbsent: aBlock