*Deprecated60
propertyValueAt: propName ifAbsent: aBlock

	self
		deprecated: 'Please use #propertyAt:ifAbsent:  instead'
		transformWith: '`@receiver propertyValueAt: `@statements1 ifAbsent: `@statements2' 
						-> '`@receiver propertyAt: `@statements1 ifAbsent: `@statements2'.

	^self propertyAt: propName ifAbsent: aBlock