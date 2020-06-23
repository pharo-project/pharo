Extension { #name : #Variable }

{ #category : #'*Deprecated90' }
Variable >> isSpecialVariable [
	self 
		deprecated: 'Use #isReservedVariable instead.' 
		transformWith: '`@receiver isSpecialVariable' -> '`@receiver isReservedVariable'.
	
	^ self isReservedVariable
]

{ #category : #'*Deprecated90' }
Variable >> variable [
	
	self 
		deprecated: 'Use receiver directly instead.' 
		transformWith: '`@receiver binding variable' -> '`@receiver binding'.
		
	^self
]
