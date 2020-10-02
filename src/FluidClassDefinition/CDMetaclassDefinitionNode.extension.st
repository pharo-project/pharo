Extension { #name : #CDMetaclassDefinitionNode }

{ #category : #'*FluidClassDefinition' }
CDMetaclassDefinitionNode >> isClassSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes.
	We named this method like that and not as isClassDefinition because it conflicts 
	with other method."
	
	^ true
]

{ #category : #'*FluidClassDefinition' }
CDMetaclassDefinitionNode >> isInstanceSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes."
	"We named this method like that because there is another method called isClassSideDefinition 
	that cannot be name isClassDefinition (because of conflicts with other method)."
	
	^ false
]

{ #category : #'*FluidClassDefinition' }
CDMetaclassDefinitionNode >> selector [ 

	^ tokens at: 3
]
