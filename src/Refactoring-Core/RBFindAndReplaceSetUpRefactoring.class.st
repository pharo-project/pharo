Class {
	#name : #RBFindAndReplaceSetUpRefactoring,
	#superclass : #RBFindAndReplaceRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #accessing }
RBFindAndReplaceSetUpRefactoring >> extractMethodRefactoring [
	^ RBExtractSetUpMethodRefactoring new
			setOption: #useExistingMethod 
			toUse:  [ :ref :aSelector | 
				true];
			setOption: #extractAssignment 
			toUse:  [ :ref :aSelector | 
				true];
			yourself 
]

{ #category : #accessing }
RBFindAndReplaceSetUpRefactoring >> nodesOf: methodNode [
	| combinations limit |
	limit :=  methodNode body statements first start.
	combinations := super nodesOf: methodNode.
	^ combinations select: [ :e | e first <= limit ] 
	
]

{ #category : #preconditions }
RBFindAndReplaceSetUpRefactoring >> selectorsFor: cls [
	^ (cls selectors select: [:e | e isTestSelector]) copyWithout: selector
]
