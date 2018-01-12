Extension { #name : #MetaclassForTraits }

{ #category : #'*Epicea' }
MetaclassForTraits >> asEpiceaRingDefinition [

	| baseClassDefinition |
	baseClassDefinition  := self theNonMetaClass asEpiceaRingDefinition.

	^ baseClassDefinition withMetaclass theMetaClass 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definition;
		yourself
]
