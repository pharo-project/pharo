failure tests
testRedefinedTemporary
	| class |
	class := model classNamed: #Foo.
	class 
		compile: 'someMethod | instVarName1 | instVarName1 := 4. ^instVarName1'
		classified: #(#accessing).
	self shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: class
				selector: #someMethod
				variable: 'instVarName1')