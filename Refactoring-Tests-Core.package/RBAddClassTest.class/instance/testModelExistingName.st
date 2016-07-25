failure tests
testModelExistingName
	| refactoring |
	refactoring := RBAddClassRefactoring 
				model: model
				addClass: #Foo
				superclass: Object
				subclasses: #()
				category: #'Refactory-Testing'.
	self shouldFail: refactoring