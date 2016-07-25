as yet unclassified
testLabelForSwapInstanceMethodRefersToClass
	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext methodContext.
	
	self assert: sugg label equals: 'Move to class side' translated.