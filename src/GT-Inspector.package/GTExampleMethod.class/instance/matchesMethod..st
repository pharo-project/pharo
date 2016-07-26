testing
matchesMethod: aCompiledMethod
	^ self exists
		and: [ self method = aCompiledMethod ]