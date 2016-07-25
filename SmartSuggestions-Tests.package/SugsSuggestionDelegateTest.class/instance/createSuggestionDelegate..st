private
createSuggestionDelegate: context
	| suggestion |

	suggestion  := SugsSuggestionFactory createMoveMethod .
	suggestion context: context.
	^suggestion.
	
