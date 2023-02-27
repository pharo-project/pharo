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
CDMetaclassDefinitionNode >> isClassSide [
	^ true
]

{ #category : #testing }
CDMetaclassDefinitionNode >> isInstanceSide [
	^ false
]

{ #category : #testing }
CDMetaclassDefinitionNode >> selector [

	^ tokens at: 3
]
