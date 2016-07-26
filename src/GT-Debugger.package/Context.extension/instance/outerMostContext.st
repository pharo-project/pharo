*GT-Debugger
outerMostContext
	^ self outerContext 
		ifNil: [ self ]
		ifNotNil: [ self outerContext ]