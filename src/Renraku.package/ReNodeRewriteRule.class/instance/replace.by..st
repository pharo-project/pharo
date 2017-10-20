deprecated
replace: aString by: aBlock
	
	self
		deprecated: 'Use #replace:byEvaluating: for better understandability'
		transformWith: '`@receiver replace: `@aString by: `@aBlock' 
						-> '`@receiver replace: `@aString byEvaluating: `@aBlock'.	

	self replace: aString byEvaluating: aBlock