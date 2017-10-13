tests - suggestions by selection
testSuggestionForAMessageNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext messageContext.

	expected := SugsSuggestionFactory commandsForMessage .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).