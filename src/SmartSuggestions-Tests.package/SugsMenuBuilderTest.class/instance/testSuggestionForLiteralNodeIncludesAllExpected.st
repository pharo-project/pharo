tests - suggestions by selection
testSuggestionForLiteralNodeIncludesAllExpected
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext literalContext.
	expected := SugsSuggestionFactory commandsFoLiteralNode .
	self assert:(expected allSatisfy:[:expect | suggestions includes: expect]).