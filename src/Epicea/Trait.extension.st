Extension { #name : #Trait }

{ #category : #'*Epicea-Ring' }
Trait >> asEpiceaRingDefinition [

	^ self asRingDefinition
		package: (EpPlatform current packageNameFor: self);
		yourself.
]
