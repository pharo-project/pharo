private
createSuggestionAccesorsClassVar: context
	| suggestion |

	suggestion  := SugsSuggestionFactory createAccessors.
	suggestion context: context.
	^suggestion.