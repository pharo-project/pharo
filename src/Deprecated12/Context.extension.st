Extension { #name : 'Context' }

{ #category : '*Deprecated12' }
Context >> lookupTempVar: aSymbol [

	self
		deprecated: 'Use lookupVar: instead'
		transformWith: '`@receiver lookupTempVar: `@argument'
			-> '`@receiver lookupVar: `@argument'.
	^ self lookupVar: aSymbol
]
