asserting
shouldnt: aBlock raise: anExceptionalEvent description: aString  
 
	^self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
		description: aString
			