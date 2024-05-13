"
I'm a composite that evaluates all the applicability preconditions and breaking changes of all the composed refactorings upfront e.g. before performing the actual sequence of refactorings. 

So do not use me if your sequence has side effects you rely on later on the following refactorings.
"
Class {
	#name : 'ReUpFrontPreconditionCheckingCompositeRefactoring',
	#superclass : 'ReCompositeRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
ReUpFrontPreconditionCheckingCompositeRefactoring >> applicabilityPreconditions [
	"Return the list of the all the applicabilityPreconditions of the composite"
	
	^ refactorings flatCollect: [ :each |  each applicabilityPreconditions ] 
]

{ #category : 'preconditions' }
ReUpFrontPreconditionCheckingCompositeRefactoring >> breakingChangePreconditions [
	"Return the list of the all the breakingChangePreconditions of the composite"

	^ refactorings flatCollect: [ :each |
		  each breakingChangePreconditions ]
]

{ #category : 'accessing' }
ReUpFrontPreconditionCheckingCompositeRefactoring >> privateTransform [
	"pay attention we are not checking preconditions of children at this level"
	
	refactorings do: [ :each | each privateTransform ]
]

{ #category : 'accessing' }
ReUpFrontPreconditionCheckingCompositeRefactoring >> violators [ 
	^ self breakingChangePreconditions flatCollect: [ :cond | cond violators ]
]
