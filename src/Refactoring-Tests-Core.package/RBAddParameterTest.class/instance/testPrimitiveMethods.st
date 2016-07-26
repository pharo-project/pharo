failure tests
testPrimitiveMethods
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBAddParameterRefactoring 
				model: model
				addParameterToMethod: #foo
				in: Object
				newSelector: #foo123124321s:
				initializer: '1'.
	self shouldFail: refactoring.
	refactoring := RBAddParameterRefactoring 
				addParameterToMethod: #at:
				in: Object
				newSelector: #at:foo:
				initializer: '1'.
	self shouldFail: refactoring.
	