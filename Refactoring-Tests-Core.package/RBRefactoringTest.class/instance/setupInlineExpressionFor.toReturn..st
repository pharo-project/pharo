set up
setupInlineExpressionFor: aRefactoring toReturn: aBoolean 
	| options |
	options := aRefactoring options copy.
	options at: #inlineExpression put: [:ref :string | aBoolean].
	aRefactoring options: options