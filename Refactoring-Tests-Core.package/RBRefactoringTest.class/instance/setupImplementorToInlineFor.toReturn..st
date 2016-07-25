set up
setupImplementorToInlineFor: aRefactoring toReturn: anObject 
	| options |
	options := aRefactoring options copy.
	options at: #implementorToInline put: [:ref :imps | anObject].
	aRefactoring options: options