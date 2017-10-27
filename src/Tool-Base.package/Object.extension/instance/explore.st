*Tool-Base
explore
	self
		deprecated: 'Please use #inspect instead'
		transformWith: '`@receiver explore' -> '`@receiver inspect'.
	
	^self inspect