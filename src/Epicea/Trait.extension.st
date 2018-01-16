Extension { #name : #Trait }

{ #category : #'*Epicea' }
Trait >> asEpiceaRingDefinition [
	
	^ (self ensureRing2DefinitionIn: RG2Environment new) asEpiceaRingDefinition
]
