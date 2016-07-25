as yet unclassified
testLabelForSwapWithoutSelectedMethodIsGeneric
	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext new.
	
	self assert: sugg label equals: 'Swap method' translated.