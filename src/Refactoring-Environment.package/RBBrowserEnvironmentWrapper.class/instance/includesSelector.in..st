testing
includesSelector: aSelector in: aClass
	^(self includesClass: aClass)
		and: [environment includesSelector: aSelector in: aClass]