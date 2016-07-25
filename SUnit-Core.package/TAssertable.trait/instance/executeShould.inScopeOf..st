asserting
executeShould: aBlock inScopeOf: anExceptionalEvent 
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: true]
			