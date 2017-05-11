*Deprecated60
propertyValueAt: propName

	self
		deprecated: 'Please use #propertyAt: instead'
		transformWith: '`@receiver propertyValueAt: `@statements1' 
						-> '`@receiver propertyAt: `@statements1'.	

	^ self propertyAt: propName
		