properties-compatibility
propertyValueAt: aKey ifAbsent: aBlock
	"use the version without ..Value, this methid is retained for compatibility"
	^self propertyAt: aKey ifAbsent: aBlock