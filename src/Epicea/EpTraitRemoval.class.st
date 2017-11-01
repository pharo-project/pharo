"
I represent the removal of a trait.
"
Class {
	#name : #EpTraitRemoval,
	#superclass : #EpTraitChange,
	#instVars : [
		'trait'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpTraitRemoval class >> trait: aTrait [

	^ self basicNew 
		initializeWithTrait: aTrait;
		yourself
]

{ #category : #visitor }
EpTraitRemoval >> accept: aVisitor [

	^ aVisitor visitTraitRemoval: self 
]

{ #category : #accessing }
EpTraitRemoval >> behaviorAffected [

	^ self traitRemoved
]

{ #category : #accessing }
EpTraitRemoval >> behaviorAffectedName [

	^ self traitRemoved name
]

{ #category : #initialization }
EpTraitRemoval >> initializeWithTrait: aTrait [

	self initialize.
	trait := aTrait asEpiceaRingDefinition.
]

{ #category : #accessing }
EpTraitRemoval >> traitRemoved [

	^ trait
]
