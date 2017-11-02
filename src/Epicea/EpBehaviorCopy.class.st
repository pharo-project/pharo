"
I represent a copy of a class or a trait .
"
Class {
	#name : #EpBehaviorCopy,
	#superclass : #EpEvent,
	#instVars : [
		'definition',
		'newName'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpBehaviorCopy class >> fromBehavior: aClass newName: aName [ 

	^ self basicNew
		initializeWithBehavior: aClass 
		newName: aName;
		yourself
]

{ #category : #visitor }
EpBehaviorCopy >> accept: aVisitor [ 
	^ aVisitor visitBehaviorCopy: self
]

{ #category : #accessing }
EpBehaviorCopy >> definition [
	
	^ definition
]

{ #category : #initialize }
EpBehaviorCopy >> initializeWithBehavior: aClass newName: aName [
	self initialize.
	
	definition := aClass asEpiceaRingDefinition.
	newName := aName asSymbol
]

{ #category : #accessing }
EpBehaviorCopy >> newName [
	
	^ newName
]
