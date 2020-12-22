Class {
	#name : #CDMetaclassDefinitionNode,
	#superclass : #CDBehaviorDefinitionNode,
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDMetaclassDefinitionNode >> existingClassIfAbsent: aBlock [
	| existingClass |
	existingClass := super existingClassIfAbsent: aBlock.	
	^existingClass classSide
]

{ #category : #testing }
CDMetaclassDefinitionNode >> isClassSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes.
	We named this method like that and not as isClassDefinition because it conflicts 
	with other method."
	
	^ true
]

{ #category : #testing }
CDMetaclassDefinitionNode >> isInstanceSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes."
	"We named this method like that because there is another method called isClassSideDefinition 
	that cannot be name isClassDefinition (because of conflicts with other method)."
	
	^ false
]

{ #category : #testing }
CDMetaclassDefinitionNode >> selector [ 

	^ tokens at: 3
]
