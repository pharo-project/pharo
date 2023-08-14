"
I represent a sequence of refactorings that are executed one after the others.
I will fail on the first one that fails. 
"
Class {
	#name : #RBCompositeRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'refactorings'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBCompositeRefactoring >> applicabilityConditions [

	^ RBCondition true 
]

{ #category : #preconditions }
RBCompositeRefactoring >> breakingChangeConditions [ 

	^ RBCondition true 
]

{ #category : #preconditions }
RBCompositeRefactoring >> generateChanges [
	
	self applicabilityConditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityConditions errorString ].
	self breakingChangeConditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangeConditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : #preconditions }
RBCompositeRefactoring >> preconditions [

	^ self applicabilityConditions & self breakingChangeConditions 
]

{ #category : #preconditions }
RBCompositeRefactoring >> privateTransform [

	refactorings do: [ :each | each primitiveExecute ]
]

{ #category : #accessing }
RBCompositeRefactoring >> refactorings [

	^ refactorings
]

{ #category : #accessing }
RBCompositeRefactoring >> refactorings: aCollection [

	refactorings := aCollection
]
