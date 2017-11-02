"
I represent the creation of a class.
"
Class {
	#name : #EpClassAddition,
	#superclass : #EpClassChange,
	#instVars : [
		'class'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpClassAddition class >> class: aClass [

	^ self basicNew 
		initializeClass: aClass;
		yourself
]

{ #category : #visitor }
EpClassAddition >> accept: aVisitor [

	^ aVisitor visitClassAddition: self
]

{ #category : #accessing }
EpClassAddition >> behaviorAffected [

	^ class
]

{ #category : #accessing }
EpClassAddition >> behaviorAffectedName [

	^ class name
]

{ #category : #accessing }
EpClassAddition >> classAdded [

	^ class 
]

{ #category : #initialization }
EpClassAddition >> initializeClass: aClass [
	self initialize.
	class := aClass asEpiceaRingDefinition
]
