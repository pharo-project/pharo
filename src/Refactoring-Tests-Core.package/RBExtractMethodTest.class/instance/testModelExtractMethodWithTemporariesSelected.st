tests
testModelExtractMethodWithTemporariesSelected
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class compile: 'foo [| temp | temp := 5. temp * temp] value'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring 
				model: model
				extract: (6 to: 36)
				from: #foo
				in: class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo [self foobar] value').
	self assert: (class parseTreeFor: #foobar) 
				= (RBParser parseMethod: 'foobar |temp | temp := 5. ^temp * temp')