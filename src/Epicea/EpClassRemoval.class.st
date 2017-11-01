"
I represent the removal of a class.
"
Class {
	#name : #EpClassRemoval,
	#superclass : #EpClassChange,
	#instVars : [
		'classRemoved'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpClassRemoval class >> class: aClassRemoved [

	^ self basicNew 
		initializeClassRemoved: aClassRemoved;
		yourself
]

{ #category : #visitor }
EpClassRemoval >> accept: aVisitor [

	^ aVisitor visitClassRemoval: self 
]

{ #category : #accessing }
EpClassRemoval >> behaviorAffected [
	^ classRemoved
]

{ #category : #accessing }
EpClassRemoval >> behaviorAffectedName [

	^ self behaviorAffected ifNotNil: #name
]

{ #category : #accessing }
EpClassRemoval >> classRemoved [

	^ classRemoved
]

{ #category : #testing }
EpClassRemoval >> doesOverride: aCodeChange [

	^ (super doesOverride: aCodeChange) or: [
		(aCodeChange isEpBehaviorChange or: [ aCodeChange isEpMethodChange ]) and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]]
]

{ #category : #initialization }
EpClassRemoval >> initializeClassRemoved: aRemovedClass [

	self initialize.
	classRemoved := aRemovedClass asEpiceaRingDefinition.

]
