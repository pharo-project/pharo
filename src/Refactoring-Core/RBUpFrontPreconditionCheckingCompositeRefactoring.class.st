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
	"basically this is checking up front all preconditions and I need a way to not rexecute them 
	when executing the refactoring.
	"

	""

	refactorings ifEmpty: [ ^ self ].
	refactorings size = 1 ifTrue: [ ^ refactorings first applicabilityPreconditions ].
	^ refactorings inject: RBTrueCondition new into: [ :sum :each | sum & each applicabilityPreconditions ]
]

{ #category : 'preconditions' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> breakingChangePreconditions [
	"basically this is checking up front all preconditions and I need a way to not rexecute them 
	when executing the refactoring.
	"

	"^ refactorings allSatisfy: [ :each | each applicabilityPreconditions check ] this is bad since it does not return a condition"

	refactorings ifEmpty: [ ^ self ].
	refactorings size = 1 ifTrue: [ ^ refactorings first breakingChangePreconditions ].
	^ refactorings allButFirst 
		inject: refactorings first breakingChangePreconditions 
		into: [ :sum :each | sum & each applicabilityPreconditions ]
]

{ #category : 'preconditions' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> checkApplicabilityPreconditions [

	| pre |
	pre := self applicabilityPreconditions.
	pre check ifFalse: [
		RBRefactoringError signal: pre errorString ].
]

{ #category : 'accessing' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> privateTransform [
	"pay attention we are not checking preconditions of children at this level"
	
	refactorings do: [ :each | each privateTransform ]
]

{ #category : 'accessing' }
RBUpFrontPreconditionCheckingCompositeRefactoring >> violators [ 
	^ self breakingChangePreconditions violators
]
