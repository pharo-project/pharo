class
browseInstVarDefsOf: aClass

	(aClass notNil and: [aClass isTrait not])
		ifTrue: [self systemNavigation browseInstVarDefs: aClass]