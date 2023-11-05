Extension { #name : 'Trait' }

{ #category : '*Epicea-Ring' }
Trait >> asEpiceaRingDefinition [

	^ self asRingDefinition
		  package: self package asRingDefinition;
		  yourself
]
