Extension { #name : #Metaclass }

{ #category : #'*Epicea-Ring' }
Metaclass >> asEpiceaRingDefinition [

	| baseClassDefinition |
	baseClassDefinition  := self instanceSide asEpiceaRingDefinition.

	^ baseClassDefinition withMetaclass classSide 
		traitCompositionSource: self classSide traitCompositionString;
		definitionSource: self classSide oldDefinition;
		addInstanceVariables: self classSide instVarNames;
		yourself
]
