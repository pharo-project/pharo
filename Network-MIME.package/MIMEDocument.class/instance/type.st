accessing
type
	"Deprecated. Use contentType instead."

	self
		deprecated: 'Please use #contentType instead'
		transformWith: '`@receiver type' -> '`@receiver contentType'.

	^ self contentType