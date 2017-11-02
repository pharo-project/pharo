Extension { #name : #TClass }

{ #category : #'*Epicea-Ring' }
TClass >> asEpiceaRingDefinition [
	"Answer a Ring definition as Epicea requires it (customized for serialization)."

	^ self explicitRequirement
]
