*Deprecated60
method: aCompiledMethod

	self
		deprecated: 'Please use #setMethod: instead'
		transformWith: '`@receiver setMethod: `@statements' -> '`@receiver setMethod: `@statements'.
		
	self setMethod: aCompiledMethod