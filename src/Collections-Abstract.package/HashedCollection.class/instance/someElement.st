accessing
someElement
	"Deprecated. Use anyOne."

	self
		deprecated: 'Please use #anyOne instead'
		transformWith: '`@receiver someElement' -> '`@receiver anyOne'.

	^ self anyOne