tests
testLabelForSwapClassMethodRefersToInstance

	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext methodClassContext.
	
	self assert: sugg label equals: 'Move to instance side' translated.