*Deprecated60
ifNotEmptyDo: notEmptyBlock ifEmpty: emptyBlock
	self
		deprecated: 'Please use #ifNotEmptyDo:ifEmpty: instead'
		transformWith: '`@receiver ifNotEmptyDo: `@statements1 ifEmpty: `@statements2' 
						-> '`@receiver ifNotEmpty: `@statements1 ifEmpty: `@statements2'.
	
	^self ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock