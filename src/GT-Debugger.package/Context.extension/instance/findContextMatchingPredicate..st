*GT-Debugger
findContextMatchingPredicate: aPredicate
	^ self findContextSuchThat: [ :ctx |
		aPredicate matches: ctx ]