Extension { #name : #Variable }

{ #category : #'*Deprecated90' }
Variable >> variable [
	
	self 
		deprecated: 'Use receiver directly instead.' 
		transformWith: '`@receiver binding variable' -> '`@receiver binding'.
		
	^self
]
