Extension { #name : #TApplyingOnClassSide }

{ #category : #'*Epicea-Ring' }
TApplyingOnClassSide >> asEpiceaRingDefinition [
	"Answer a Ring definition as Epicea requires it (customized for serialization)."

	^ self explicitRequirement
]
