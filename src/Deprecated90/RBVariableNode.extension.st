Extension { #name : #RBVariableNode }

{ #category : #'*Deprecated90' }
RBVariableNode >> isSpecialVariable [
	self 
		deprecated: 'Use #isReservedVariable instead.' 
		transformWith: '`@receiver isSpecialVariable' -> '`@receiver isReservedVariable'.
	
	^ self isReservedVariable
]
