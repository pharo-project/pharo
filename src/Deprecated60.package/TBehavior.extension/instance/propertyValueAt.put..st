*Deprecated60
propertyValueAt: propName put: propValue
	self
		deprecated: 'Please use #propertyAt:put:  instead'
		transformWith: '`@receiver propertyValueAt: `@statements1 put: `@statements2' 
						-> '`@receiver propertyAt: `@statements1 put: `@statements2'.

	^ self propertyAt: propName put: propValue