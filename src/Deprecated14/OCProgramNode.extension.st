Extension { #name : 'OCProgramNode' }

{ #category : '*Deprecated14' }
OCProgramNode >> hasLinks [
	self
		deprecated: 'use #hasMetaLinks'
		transformWith:  '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^self hasMetaLinks
]

{ #category : '*Deprecated14' }
OCProgramNode >> isReservedVariable [
	self
		deprecated: 'Please use #isPseudoVariable instead'
		transformWith: '`@receiver isReservedVariable' -> '`@receiver isPseudoVariable'.

	^ self isPseudoVariable
]
