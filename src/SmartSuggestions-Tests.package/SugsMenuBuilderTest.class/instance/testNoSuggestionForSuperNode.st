tests - suggestions by selection
testNoSuggestionForSuperNode
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext superContext.
	expected := SugsSuggestionFactory globalCommands .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).