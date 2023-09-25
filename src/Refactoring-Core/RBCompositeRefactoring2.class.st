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
	
	"^ refactorings allSatisfy: [ :each | each applicabilityPreconditions check ] this is bad since it does not return a condition"
	
	^ refactorings inject: RBTrueCondition new into: [ :sum :each | sum & each applicabilityPreconditions ]
	
]

{ #category : 'preconditions' }
RBCompositeRefactoring2 >> breakingChangePreconditions [ 
	"basically this is checking up front all preconditions and I need a way to not rexecute them 
	when executing the refactoring.
	"
	
	"^ refactorings allSatisfy: [ :each | each applicabilityPreconditions check ] this is bad since it does not return a condition"
	
	^ refactorings inject: RBTrueCondition new into: [ :sum :each | sum & each breakingChangePreconditions ]
	
]

{ #category : 'accessing' }
RBCompositeRefactoring2 >> violators [ 

	^ self breakingChangePreconditions violators "flatCollect: [ :each | each violators ]"
]
