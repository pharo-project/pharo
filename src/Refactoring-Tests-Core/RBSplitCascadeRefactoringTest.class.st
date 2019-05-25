"
Test refactoring for splitting a cascade message send in multiple messages.

The method #methodWithCascades is not called, it is just a ""test data"" method used as target for the refactoring operation.
"
Class {
	#name : #RBSplitCascadeRefactoringTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #data }
RBSplitCascadeRefactoringTest >> methodWithCascades [
	| a |
	a := Object new initialize; asString.
	^ a
	
]

{ #category : #tests }
RBSplitCascadeRefactoringTest >> testSplitCascadeRefactoring [
	| refactoring |
	refactoring := RBSplitCascadeRefactoring
		split: (54 to: 55)
		from: #methodWithCascades
		in: self class.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBSplitCascadeRefactoringTest) parseTreeFor: #methodWithCascades) equals: (self parseMethod: 'methodWithCascades
	| a receiver |
	receiver := Object new.
	receiver initialize.
	a := receiver asString.
	^ a')
]
