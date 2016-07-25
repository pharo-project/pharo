lookup
lookupVar: name inContext: aContext

	^ self outerScope lookupVar: name inContext: aContext outerContext