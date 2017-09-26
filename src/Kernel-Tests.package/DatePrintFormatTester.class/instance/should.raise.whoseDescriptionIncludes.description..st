asserting
should: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: substring description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: substring)
		description: aString
