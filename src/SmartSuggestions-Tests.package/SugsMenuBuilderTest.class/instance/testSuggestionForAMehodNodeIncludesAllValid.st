tests - suggestions by selection
testSuggestionForAMehodNodeIncludesAllValid
	| suggestions expected context |

	context := SugsMockContext methodContext.
	suggestions := SugsMenuBuilder findSuggestionsFor: context.
	expected := (SugsSuggestionFactory commandsForMethod do: [:aSugg | aSugg context: context] )select: #isValidForContext.
	self assert: (expected allSatisfy: [ :expect | suggestions includes: expect ])