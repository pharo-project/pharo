failure tests
testMovePrimitiveMethod
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBMoveMethodRefactoring 
				model: model
				selector: #foo
				class: Object
				variable: 'OrderedCollection'.
	self shouldFail: refactoring