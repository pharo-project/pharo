tests - suggestions by selection
testSuggestionForAAssigmentNodeIncludesAllDeclared
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext assigmentContext.

	expected := SugsSuggestionFactory commandsForAssignment.	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).