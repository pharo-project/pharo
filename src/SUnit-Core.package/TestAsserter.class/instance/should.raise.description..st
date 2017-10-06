asserting
should: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString
			