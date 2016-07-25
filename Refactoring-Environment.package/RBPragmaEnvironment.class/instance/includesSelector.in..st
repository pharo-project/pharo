testing
includesSelector: aSelector in: aClass
	(environment includesSelector: aSelector in: aClass)
		ifFalse: [ ^ false ].
	^ (aClass compiledMethodAt: aSelector) pragmas
		anySatisfy: [ :each | self includesPragma: each ]