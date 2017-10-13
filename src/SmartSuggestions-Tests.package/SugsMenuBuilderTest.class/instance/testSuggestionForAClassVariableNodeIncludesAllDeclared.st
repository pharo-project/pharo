tests - suggestions by selection
testSuggestionForAClassVariableNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext classVariableContext.

	expected := SugsSuggestionFactory commandsForClassVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).