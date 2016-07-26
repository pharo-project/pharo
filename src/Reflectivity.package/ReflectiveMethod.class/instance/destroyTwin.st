invalidate
destroyTwin
	(ast hasProperty: #wrappedPrimitive) ifTrue: [  ast :=  compiledMethod parseTree].
	self recompileAST.
	self installCompiledMethod.
	compiledMethod reflectiveMethod: nil.
	SystemAnnouncer uniqueInstance unsubscribe: self