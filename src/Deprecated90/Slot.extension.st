Extension { #name : #Slot }

{ #category : #'*Deprecated90' }
Slot >> isLocal [
	self 
		deprecated: 'Use #isDefinedByOwningClass instead.' 
		transformWith: '`@receiver isLocal`' -> '`@receiver isDefinedByOwningClass'.
		
	^ self isDefinedByOwningClass 
]
