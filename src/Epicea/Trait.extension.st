Extension { #name : #Trait }

{ #category : #'*Epicea' }
Trait >> asEpiceaRingDefinition [

	^ self asRing2Definition
		package: (EpPlatform current packageNameFor: self);
		yourself.
]
