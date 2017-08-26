deprecated
isCritic

	self
		deprecated: 'Please use #isCritique instead'
		transformWith: '`@receiver isCritic' -> '`@receiver isCritique'.
	^ self isCritique