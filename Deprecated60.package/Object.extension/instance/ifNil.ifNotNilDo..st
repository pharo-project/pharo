*Deprecated60
ifNil: nilBlock ifNotNilDo: aBlock
	self
		deprecated: 'Please use #ifNil:ifNotNil:  instead'
		transformWith: '`@receiver ifNil: `@statements1 ifNotNilDo: `@statements2' 
						-> '`@receiver ifNil: `@statements1 ifNotNil: `@statements2'.
	
	^ self ifNil: nilBlock ifNotNil: aBlock