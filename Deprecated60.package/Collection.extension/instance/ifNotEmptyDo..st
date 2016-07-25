*Deprecated60
ifNotEmptyDo: aBlock
	self
		deprecated: 'Please use #ifNotEmpty: instead'
		transformWith: '`@receiver ifNotEmptyDo: `@statements' -> '`@receiver ifNotEmpty: `@statements'.

	^ self ifNotEmpty: aBlock