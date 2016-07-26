asserting
executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: aString
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubstring: aString) ]
			