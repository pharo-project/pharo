asserting
should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
			