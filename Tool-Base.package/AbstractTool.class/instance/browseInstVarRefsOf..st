class
browseInstVarRefsOf: aClass

	(aClass notNil and: [aClass isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: aClass]