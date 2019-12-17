"
I represent the modification of a trait.
"
Class {
	#name : #EpTraitModification,
	#superclass : #EpTraitChange,
	#instVars : [
		'oldTrait',
		'newTrait'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpTraitModification class >> oldTrait: anOldTrait newTrait: aNewTrait [

	^ self basicNew 
		initializeOldTrait: anOldTrait newTrait: aNewTrait;
		yourself
]

{ #category : #visitor }
EpTraitModification >> accept: aVisitor [

	^ aVisitor visitTraitModification: self 
]

{ #category : #accessing }
EpTraitModification >> behaviorAffected [
	^ newTrait
]

{ #category : #accessing }
EpTraitModification >> behaviorAffectedName [

	^ newTrait name
]

{ #category : #testing }
EpTraitModification >> hasChanges [
	^ oldTrait definitionSource ~= newTrait definitionSource or: [ 
		oldTrait classSide definitionSource ~= newTrait classSide definitionSource ]
]

{ #category : #initialize }
EpTraitModification >> initializeOldTrait: anOldTrait newTrait: aNewTrait [

	self initialize.
	oldTrait := anOldTrait asEpiceaRingDefinition.
	newTrait := aNewTrait asEpiceaRingDefinition.
]

{ #category : #accessing }
EpTraitModification >> newTrait [

	^ newTrait
]

{ #category : #accessing }
EpTraitModification >> oldTrait [

	^ oldTrait
]
