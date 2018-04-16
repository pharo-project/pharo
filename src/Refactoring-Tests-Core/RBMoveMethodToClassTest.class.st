Class {
	#name : #RBMoveMethodToClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBMoveMethodToClassTest >> testMethodAlreadyDefined [
	| method someClass |
	someClass := model classNamed: #RBFooLintRuleTest.
	method := RBClassModelFactory rbMethod for: someClass source: 'foo' , String newLine , String tab , '^ 6' selector: #foo.
	self shouldFail: (RBMoveMethodToClassRefactoring method: method class: someClass)
]
