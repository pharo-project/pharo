transforming
inlineForwarder
	| refactoring |
	refactoring := RBInlineAllSendersRefactoring 
				model: self model
				sendersOf: extractedMethodSelector
				in: class.
	refactoring setOption: #inlineExpression toUse: [:ref :string | true].
	self performComponentRefactoring: refactoring