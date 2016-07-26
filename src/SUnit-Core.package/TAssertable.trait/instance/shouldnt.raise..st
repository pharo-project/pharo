asserting
shouldnt: aBlock raise: anExceptionalEvent  
	 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
			