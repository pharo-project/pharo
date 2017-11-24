"
I represent the creation of a trait.
"
Class {
	#name : #EpTraitAddition,
	#superclass : #EpTraitChange,
	#instVars : [
		'trait'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpTraitAddition class >> trait: aTrait [

	^ self basicNew
		initializeWithTrait: aTrait;
		yourself 
]

{ #category : #visitor }
EpTraitAddition >> accept: aVisitor [

	^ aVisitor visitTraitAddition: self 
]

{ #category : #accessing }
EpTraitAddition >> behaviorAffected [
	^ self traitAdded
]

{ #category : #accessing }
EpTraitAddition >> behaviorAffectedName [

	^ self traitAdded name
]

{ #category : #initialization }
EpTraitAddition >> initializeWithTrait: aTrait [

	self initialize.
	trait := aTrait asEpiceaRingDefinition.
]

{ #category : #accessing }
EpTraitAddition >> traitAdded [

	^ trait 
]
