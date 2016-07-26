invalidate
installCompiledMethod
	self installMethod: compiledMethod.
	ASTCache default at: compiledMethod put: ast.