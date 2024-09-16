Extension { #name : 'Context' }

{ #category : '*Deprecated12' }
Context >> hasMethodReturn [
	self
		deprecated: 'use #hasNonLocalReturn'
		transformWith: '`@receiver hasMethodReturn' -> '`@receiver hasNonLocalReturn'.
	^self hasNonLocalReturn
]

{ #category : '*Deprecated12' }
Context >> lookupTempVar: aSymbol [

	self
		deprecated: 'Use lookupVar: instead'
		transformWith: '`@receiver lookupTempVar: `@argument'
			-> '`@receiver lookupVar: `@argument'.
	^ self lookupVar: aSymbol
]

{ #category : '*Deprecated12' }
Context >> methodReturnContext [
	self
		deprecated: 'Use #home instead'
		transformWith: '`@receiver methodReturnContext' -> '`@receiver home'.
	^ self home
]

{ #category : '*Deprecated12' }
Context >> methodSelector [
	self
		deprecated: 'use #selector'
		transformWith: '`@receiver methodSelector' -> '`@receiver selector'.
	^ self selector
]
