suggstions-by-selection
testSuggestionForAMessageNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext mesageContext.

	expected := SugsSuggestionFactory commandsForMessage .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).