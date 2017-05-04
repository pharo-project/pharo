*Deprecated60
getSource
	self
		deprecated: 'Please use #sourceCode instead'
		transformWith: '`@receiver getSource' -> '`@receiver sourceCode'.
	^self sourceCode.