*Deprecated60
correctCount
	
	self
		deprecated: 'Please use #passedCount instead'
		transformWith: '`@receiver correctCount' 
						-> '`@receiver passedCount'.	
	
	^self passedCount