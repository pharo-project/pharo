tests - suggestions by selection
testSuggestionForSourceNodeIncludesAllValid
	| suggestions expected context |
	context := SugsMockContext sourceContext.
	suggestions :=SugsMenuBuilder findSuggestionsFor: context.
	expected := (SugsSuggestionFactory commandsForSource do: [:aSugg | aSugg context: context] )select: #isValidForContext.
		
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).