accessing
selector
	"Answer the selector of the method containing the pragma.
	 Do not confuse this with the selector of the pragma's message pattern."
	
	self
		deprecated: 'Please use #methodSelector instead'
		transformWith: '`@receiver selector' -> '`@receiver methodSelector'.
	^ self methodSelector