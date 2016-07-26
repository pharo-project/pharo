testing
includesSelector: aSelector in: aClass 
	^(environment includesSelector: aSelector in: aClass) 
		and: [andedEnvironment includesSelector: aSelector in: aClass]