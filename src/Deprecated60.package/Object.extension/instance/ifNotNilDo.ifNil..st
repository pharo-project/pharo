*Deprecated60
ifNotNilDo: aBlock ifNil: nilBlock
	self
		deprecated: 'Please use #ifNotNil:ifNil: instead'
		transformWith: '`@receiver ifNotNilDo: `@statements1 ifNil: `@statements2'
						 -> '`@receiver ifNotNil: `@statements1 ifNil: `@statements2'.

	^ self ifNotNil: aBlock ifNil: nilBlock
