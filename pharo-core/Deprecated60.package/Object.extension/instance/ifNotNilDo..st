*Deprecated60
ifNotNilDo: aBlock
	self
		deprecated: 'Please use #ifNotNil: instead'
		transformWith: '`@receiver ifNotNilDo: `@statements' -> '`@receiver ifNotNil: `@statements'.
	^ self ifNotNil: aBlock