"
I represent a refactoring that is composed of other refactorings.
"
Class {
	#name : #EpCompositeRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'childrenRefactorings'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpCompositeRefactoring class >> withAll: someRefactorings [
	^ self basicNew 	
		initializeWith: someRefactorings;
		yourself
]

{ #category : #converting }
EpCompositeRefactoring >> asRBRefactoring [
	^ RBCompositeRefactoryChange new
		changes: (childrenRefactorings collect: #asRBRefactoring);
		yourself
]

{ #category : #accessing }
EpCompositeRefactoring >> childrenRefactorings [
	^ childrenRefactorings
]

{ #category : #initializing }
EpCompositeRefactoring >> initializeWith: someRefactorings [ 
	self initialize.
	childrenRefactorings := someRefactorings collect: #asEpiceaEvent
]
