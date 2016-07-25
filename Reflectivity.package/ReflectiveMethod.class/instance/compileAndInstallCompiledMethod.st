evaluation
compileAndInstallCompiledMethod
	(compiledMethod isRealPrimitive and: [ast hasMetalink]) ifTrue: [ self generatePrimitiveWrapper ].
	self recompileAST.
	self installCompiledMethod.