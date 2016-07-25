*Deprecated60
ifEmpty: emptyBlock ifNotEmptyDo: notEmptyBlock

	self
		deprecated: 'Please use #ifEmpty:ifNotEmptyDo: instead'
		transformWith: '`@receiver ifEmpty: `@statements1 ifNotEmptyDo: `@statements2' 
						-> '`@receiver ifEmpty: `@statements1 ifNotEmpty: `@statements2'.
						
	^self ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock