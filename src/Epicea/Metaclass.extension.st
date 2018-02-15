Extension { #name : #Metaclass }

{ #category : #'*Epicea-Ring' }
Metaclass >> asEpiceaRingDefinition [

	| baseClassDefinition |
	baseClassDefinition  := self theNonMetaClass asEpiceaRingDefinition.

	^ baseClassDefinition withMetaclass classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide definition;
		addInstanceVariables: self classSide instVarNames;
		yourself
]
