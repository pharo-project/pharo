Extension { #name : #CDBehaviorDefinitionNode }

{ #category : #'*FluidClassDefinition' }
CDBehaviorDefinitionNode >> classKind [

	^ classKind
]

{ #category : #'*FluidClassDefinition' }
CDBehaviorDefinitionNode >> hasTraitComposition [
	
	^ traitDefinition isNotNil
]

{ #category : #'*FluidClassDefinition' }
CDBehaviorDefinitionNode >> tokens [
	^ tokens
]

{ #category : #'*FluidClassDefinition' }
CDBehaviorDefinitionNode >> tokens: aCollection [ 
	tokens := aCollection
]
