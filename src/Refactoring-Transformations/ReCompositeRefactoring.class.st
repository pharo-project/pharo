"
I represent a sequence of refactorings that are executed one after the others.
I will fail on the first one that fails. 
"
Class {
	#name : 'ReCompositeRefactoring',
	#superclass : 'ReRefactoring',
	#instVars : [
		'refactorings'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
ReCompositeRefactoring >> breakingChangePreconditions [
	"Return the list of the all the breakingChangePreconditions of the composite"

	^ refactorings flatCollect: [ :each |
		  each breakingChangePreconditions ]
]

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

{ #category : 'transforming' }
ReCompositeRefactoring >> privateTransform2 [

	refactorings do: [ :each | each privateTransform ]
]

{ #category : 'accessing' }
ReCompositeRefactoring >> refactorings [

	^ refactorings
]

{ #category : 'accessing' }
ReCompositeRefactoring >> refactorings: aCollection [

	refactorings := aCollection
]
