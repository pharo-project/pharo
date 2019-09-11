Extension { #name : #MetaclassForTraits }

{ #category : #'*Epicea' }
MetaclassForTraits >> asEpiceaRingDefinition [

	| baseClassDefinition |
	baseClassDefinition  := self instanceSide asEpiceaRingDefinition.

	^ baseClassDefinition withMetaclass classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide oldDefinition;
		yourself
]
