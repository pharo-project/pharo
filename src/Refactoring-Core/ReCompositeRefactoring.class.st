"
I represent a sequence of refactorings that are executed one after the others.
I will fail on the first one that fails. 
"
Class {
	#name : 'ReCompositeRefactoring',
	#superclass : 'RBRefactoring',
	#instVars : [
		'refactorings'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
ReCompositeRefactoring >> preconditions [
	"Return the list of all the preconditions."
	
	^ self applicabilityPreconditions , self breakingChangePreconditions
]

{ #category : 'preparation' }
ReCompositeRefactoring >> prepareForInteractiveMode [
	self 
]

{ #category : 'transforming' }
ReCompositeRefactoring >> privateTransform [

	refactorings do: [ :each | each generateChanges ]
]

{ #category : 'accessing' }
ReCompositeRefactoring >> refactorings [

	^ refactorings
]

{ #category : 'accessing' }
ReCompositeRefactoring >> refactorings: aCollection [

	refactorings := aCollection
]
