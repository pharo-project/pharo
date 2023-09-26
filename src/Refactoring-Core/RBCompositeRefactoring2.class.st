Class {
	#name : 'RBCompositeRefactoring2',
	#superclass : 'RBCompositeRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBCompositeRefactoring2 >> applicabilityPreconditions [
	"basically this is checking up front all preconditions and I need a way to not rexecute them 
	when executing the refactoring.
	"
	
	
	
	"^ refactorings inject: RBTrueCondition new into: [ :sum :each | sum & each applicabilityPreconditions ]"
	
	refactorings ifEmpty: [ ^ self ].
	refactorings size = 1
		ifTrue: [ ^ refactorings first applicabilityPreconditions ].
	^ refactorings fold: [ :sum :each | sum & each applicabilityPreconditions ]
	
]

{ #category : 'preconditions' }
RBCompositeRefactoring2 >> breakingChangePreconditions [ 
	"basically this is checking up front all preconditions and I need a way to not rexecute them 
	when executing the refactoring.
	"
	
	"^ refactorings allSatisfy: [ :each | each applicabilityPreconditions check ] this is bad since it does not return a condition"
	refactorings ifEmpty: [ ^ self ].
	refactorings size = 1
		ifTrue: [ ^ refactorings first breakingChangePreconditions ].
	^ refactorings fold: [ :sum :each | sum & each breakingChangePreconditions ]
	
]

{ #category : 'accessing' }
RBCompositeRefactoring2 >> privateTransform [
	"pay attention we are not checking preconditions of children at this level"
	
	refactorings do: [ :each | each privateTransform ]
]

{ #category : 'accessing' }
RBCompositeRefactoring2 >> violators [ 

	^ self breakingChangePreconditions violators "flatCollect: [ :each | each violators ]"
]
