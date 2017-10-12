tests - suggestions by selection
testSuggestionForAnInstanceVariableNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext instVarContext.

	expected := SugsSuggestionFactory commandsForInstanceVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).