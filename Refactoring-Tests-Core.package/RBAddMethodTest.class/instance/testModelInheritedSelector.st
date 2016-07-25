failure tests
testModelInheritedSelector
	| refactoring |
	refactoring := RBAddMethodRefactoring 
				model: model
				addMethod: 'printString ^super printString'
				toClass: (model classNamed: #Bar)
				inProtocols: #(#accessing).
	self shouldFail: refactoring