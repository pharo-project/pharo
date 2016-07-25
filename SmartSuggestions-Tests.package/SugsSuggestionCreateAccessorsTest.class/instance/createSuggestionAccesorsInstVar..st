private
createSuggestionAccesorsInstVar: context
	| suggestion |

	suggestion  := SugsSuggestionFactory createAccesorsFor.
	suggestion context: context.
	^suggestion.