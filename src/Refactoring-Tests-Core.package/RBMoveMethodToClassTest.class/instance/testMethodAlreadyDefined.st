failure tests
testMethodAlreadyDefined
	| method someClass |
	someClass := model classNamed: #RBFooLintRuleTest.
	method := RBClassModelFactory rbMethod for: someClass source: 'foo' , String cr , String tab , '^ 6' selector: #foo.
	self shouldFail: (RBMoveMethodToClassRefactoring method: method class: someClass)