asserting
should: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: substring description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: substring)
		description: aString
