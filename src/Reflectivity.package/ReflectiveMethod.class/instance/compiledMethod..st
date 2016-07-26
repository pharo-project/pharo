accessing
compiledMethod: aCompiledMethod
	compiledMethod := aCompiledMethod.
	class := aCompiledMethod methodClass.
	ast := compiledMethod ast.
	SystemAnnouncer uniqueInstance when: ASTCacheReset send: #reinstallASTInCache to: self