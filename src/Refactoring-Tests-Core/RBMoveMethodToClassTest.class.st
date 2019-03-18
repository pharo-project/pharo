Class {
	#name : #RBMoveMethodToClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBMoveMethodToClassTest >> testMethodAlreadyDefined [
	| method someClass |
	someClass := model classNamed: #RBFooLintRuleTestData.
	method := RBClassModelFactory rbMethod for: someClass source: 'foo' , String cr , String tab , '^ 6' selector: #foo.
	self shouldFail: (RBMoveMethodToClassRefactoring method: method class: someClass)
]
