Extension { #name : #Behavior }

{ #category : #'*Epicea' }
Behavior >> asEpiceaRingDefinition [
	
	^ (self ensureRing2DefinitionIn: RG2Environment new) asEpiceaRingDefinition
]
