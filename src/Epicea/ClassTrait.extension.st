Extension { #name : #ClassTrait }

{ #category : #'*Epicea-Ring' }
ClassTrait >> asEpiceaRingDefinition [

	^ (self ensureRing2DefinitionIn: RG2Environment new) asEpiceaRingDefinition
]
