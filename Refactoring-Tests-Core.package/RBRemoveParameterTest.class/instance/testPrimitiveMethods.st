failure tests
testPrimitiveMethods
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo123: a <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBRemoveParameterRefactoring 
				model: model
				removeParameter: 'a'
				in: Object
				selector: #foo123:.
	self shouldFail: refactoring