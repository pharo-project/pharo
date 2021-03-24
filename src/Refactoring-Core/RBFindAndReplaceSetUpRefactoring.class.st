Class {
	#name : #RBFindAndReplaceSetUpRefactoring,
	#superclass : #RBFindAndReplaceRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring class >> model: aModel of: aClass inWholeHierarchy: aBoolean [
	^ self new
		model: aModel;
		of: aClass
		inWholeHierarchy: aBoolean;
		yourself
]

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring class >> of: aClass inWholeHierarchy: aBoolean [
	^ self new
		of: aClass
		inWholeHierarchy: aBoolean;
		yourself
]

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
RBFindAndReplaceSetUpRefactoring >> methodNode [ 
	| node |
	node := super methodNode.
	node body removeNode: (RBParser parseExpression: 'super setUp.').
	^ node
]

{ #category : #accessing }
RBFindAndReplaceSetUpRefactoring >> nodesOf: methodNode [
	| combinations limit |
	"TODO -> corregir para cuando un metodo sea vacio"
	limit :=  methodNode body statements first start.
	combinations := super nodesOf: methodNode.
	^ combinations select: [ :e | e first <= limit ] 
	
]

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring >> of: aClass inWholeHierarchy: aBoolean [
	class := self classObjectFor: aClass.
	selector := #setUp.
	replacesAllHierarchy := aBoolean.
]

{ #category : #preconditions }
RBFindAndReplaceSetUpRefactoring >> selectorsFor: cls [
	^ (cls selectors select: [:e | e isTestSelector]) copyWithout: selector
]

{ #category : #accessing }
RBFindAndReplaceSetUpRefactoring >> startLimitOf: sourceCode [
	^ ((self parserClass parseMethod: sourceCode) body statements first start)
]
