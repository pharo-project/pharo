Extension { #name : 'Variable' }

{ #category : '*Deprecated12' }
Variable >> isReservedVariable [
	self
		deprecated: 'Please use #isPseudoVariable instead'
		transformWith: '`@receiver isReservedVariable' -> '`@receiver isPseudoVariable'.

	^ self isPseudoVariable
]
