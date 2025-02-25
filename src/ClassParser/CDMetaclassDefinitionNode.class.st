Class {
	#name : 'CDMetaclassDefinitionNode',
	#superclass : 'CDBehaviorDefinitionNode',
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'accessing' }
CDMetaclassDefinitionNode >> existingClassIfAbsent: aBlock [
	| existingClass |
	existingClass := super existingClassIfAbsent: aBlock.
	^existingClass classSide
]

{ #category : 'testing' }
CDMetaclassDefinitionNode >> isClassSide [
	^ true
]

{ #category : 'testing' }
CDMetaclassDefinitionNode >> isInstanceSide [
	^ false
]
