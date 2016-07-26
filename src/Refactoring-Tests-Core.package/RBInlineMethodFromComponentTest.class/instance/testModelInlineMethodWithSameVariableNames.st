tests
testModelInlineMethodWithSameVariableNames
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class compile: 'a9: a b9: b ^self + a + b' classified: #(#accessing).
	(model classNamed: #Object) 
		compile: 'foo | a b c | a := InlineMethodFromComponentTest new. b := 1. c := 2. ^a a9: b b9: c'
		classified: #(#accessing).
	self proceedThroughWarning: 
			[refactoring := RBInlineMethodFromComponentRefactoring 
						model: model
						inline: (72 to: 84)
						inMethod: #foo
						forClass: (model classNamed: #Object).
			self setupInlineExpressionFor: refactoring toReturn: false.
			self executeRefactoring: refactoring].
	self assert: ((refactoring model classNamed: #Object) parseTreeFor: #foo) 
				= (RBParser 
						parseMethod: 'foo | a b c | a := InlineMethodFromComponentTest new. b := 1. c := 2. ^a + b + c')