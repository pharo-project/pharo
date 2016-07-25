testing
includesSelector: aSelector in: aClass 
	^(environment includesSelector: aSelector in: aClass)
		and: [self privateIncludesSelector: aSelector inClass: aClass]