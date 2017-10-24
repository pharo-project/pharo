tests - suggestions by selection
testSuggestionForAClassNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext classContext.

	expected := SugsSuggestionFactory commandsForClass.	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).