tests - suggestions by selection
testSuggestionForATemporaryVariableNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext tempVarContext.

	expected := SugsSuggestionFactory commandsForTemporaryVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).