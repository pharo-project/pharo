"
I represent a sequence of refactorings that are executed one after the others.
I will fail on the first one that fails. 
"
Class {
	#name : 'RBCompositeRefactoring',
	#superclass : 'RBRefactoring',
	#instVars : [
		'refactorings'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBCompositeRefactoring >> applicabilityPreconditions [
	"Return the list of the all the applicabilityPreconditions of the composite"
	^ #()
]

{ #category : 'preconditions' }
RBCompositeRefactoring >> breakingChangePreconditions [ 
		"Return the list of the all the breakingChangePreconditions of the composite"
		
	^ #()
]

{ #category : 'scripting api - executing' }
RBCompositeRefactoring >> generateChanges [
	
	self prepareForExecution.
	self checkApplicabilityPreconditions.
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : 'preconditions' }
RBCompositeRefactoring >> preconditions [
	"Return the list of all the preconditions."
	
	^ self applicabilityPreconditions addAll: self breakingChangePreconditions ; yourself
]

{ #category : 'preparation' }
RBCompositeRefactoring >> prepareForInteractiveMode [
	self 
]

{ #category : 'transforming' }
RBCompositeRefactoring >> privateTransform [

	refactorings do: [ :each | each generateChanges ]
]

{ #category : 'accessing' }
RBCompositeRefactoring >> refactorings [

	^ refactorings
]

{ #category : 'accessing' }
RBCompositeRefactoring >> refactorings: aCollection [

	refactorings := aCollection
]
