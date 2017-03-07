accessing
compiledMethod: aCompiledMethod

	compiledMethod := aCompiledMethod.
	class := aCompiledMethod methodClass.
	ast := compiledMethod ast.
	
	SystemAnnouncer uniqueInstance weak 
		when: ASTCacheReset 
		send: #reinstallASTInCache 
		to: self