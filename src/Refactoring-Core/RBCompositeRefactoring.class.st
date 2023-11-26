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

	^ RBCondition true 
]

{ #category : 'preconditions' }
RBCompositeRefactoring >> breakingChangePreconditions [ 

	^ RBCondition true 
]

{ #category : 'executing' }
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

	^ self applicabilityPreconditions & self breakingChangePreconditions 
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
