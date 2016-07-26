testing
appliesToDebugger: aDebugger

	^ aDebugger session class = GTSUnitDebugSession and: [ 
		aDebugger session assertionCallerContext notNil and: [ 
			aDebugger session testContext ~= aDebugger session assertionCallerContext ] ]