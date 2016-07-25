testing
includesSelector: aSelector in: aClass 
	^(super includesSelector: aSelector in: aClass) and: 
			[protocols 
				includes: (environment whichProtocolIncludes: aSelector in: aClass)]