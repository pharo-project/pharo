testing
includesSelector: aSelector in: aClass
	^ (environment includesSelector: aSelector in: aClass)
		or: [ orEnvironment includesSelector: aSelector in: aClass ]