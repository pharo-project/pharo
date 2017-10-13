tests - suggestions by selection
testNoSuggestionForSelfNode
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext selfContext.
	expected := SugsSuggestionFactory globalCommands .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).