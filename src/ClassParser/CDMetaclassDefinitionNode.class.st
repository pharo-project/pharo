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
