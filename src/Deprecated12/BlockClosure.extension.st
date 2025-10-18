Extension { #name : 'BlockClosure' }

{ #category : '*Deprecated12' }
BlockClosure >> hasMethodReturn [
	self
		deprecated: 'use #hasNonLocalReturn'
		transformWith: '`@receiver hasMethodReturn' -> '`@receiver hasNonLocalReturn'.
	^self hasNonLocalReturn
]
