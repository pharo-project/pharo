*Deprecated70
longDescription
	self
		deprecated: 'Please use #rationale  instead'
		transformWith: '`@receiver longDescription' 
						-> '`@receiver rationale'.
	
	^ self rationale