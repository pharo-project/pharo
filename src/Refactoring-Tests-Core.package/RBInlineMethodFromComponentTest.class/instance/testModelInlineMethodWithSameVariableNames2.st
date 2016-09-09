tests
testModelInlineMethodWithSameVariableNames2
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class compile: 'a9: a b9: b ^self + a + b' classified: #(#accessing).
	(model classNamed: #Object) 
		compile: 'foo | a b c | a := InlineMethodFromComponentTest new. b := 1. c := 2. ^c a9: b b9: a'
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
						parseMethod: 'foo | a b c | a := InlineMethodFromComponentTest new. b := 1. c := 2. ^c + b + a')