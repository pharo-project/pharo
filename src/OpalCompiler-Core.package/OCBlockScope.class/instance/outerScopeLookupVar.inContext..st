lookup
outerScopeLookupVar: name inContext: aContext
	^ self outerScope 
		lookupVar: name
		inContext: (self nextOuterScopeContextOf: aContext)