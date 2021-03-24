Class {
	#name : #RBFindAndReplaceSetUpRefactoring,
	#superclass : #RBFindAndReplaceRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring class >> model: aModel of: aClass inAllHierarchy: aBoolean [
	^ self new
		model: aModel;
		of: aClass
		inAllHierarchy: aBoolean;
		yourself
]

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring class >> of: aClass inAllHierarchy: aBoolean [
	^ self new
		of: aClass
		inAllHierarchy: aBoolean;
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
RBFindAndReplaceSetUpRefactoring >> nodesOf: methodNode [
	| combinations limit |
	limit :=  methodNode body statements first start.
	combinations := super nodesOf: methodNode.
	^ combinations select: [ :e | e first <= limit ] 
	
]

{ #category : #'instance creation' }
RBFindAndReplaceSetUpRefactoring >> of: aClass inAllHierarchy: aBoolean [
	class := self classObjectFor: aClass.
	selector := #setUp.
	replacesAllHierarchy := aBoolean.
]

{ #category : #preconditions }
RBFindAndReplaceSetUpRefactoring >> selectorsFor: cls [
	^ (cls selectors select: [:e | e isTestSelector]) copyWithout: selector
]
