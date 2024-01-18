"
I'm a composite that evaluates all the applicability preconditions and breaking changes of all the composed refactorings upfront e.g. before performing the actual sequence of refactorings. 

So do not use me if your sequence has side effects you rely on later on the following refactorings.
"
Class {
	#name : 'RBUpFrontPreconditionCheckingCompositeRefactoring',
	#superclass : 'RBCompositeRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> applicabilityPreconditions [
	"Return the list of the all the applicabilityPreconditions of the composite"
	
	^ refactorings collect: [ :each |  each applicabilityPreconditions ] 
]

{ #category : 'preconditions' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> breakingChangePreconditions [
	"Return the list of the all the breakingChangePreconditions of the composite"
	
	^ refactorings collect: [ :each | each breakingChangePreconditions ]
]

{ #category : 'accessing' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> privateTransform [
	"pay attention we are not checking preconditions of children at this level"
	
	refactorings do: [ :each | each privateTransform ]
]

{ #category : 'accessing' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> violators [ 
	^ self breakingChangePreconditions flatCollect: [ :cond | cond violators ]
]
