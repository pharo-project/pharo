tools
nameAnalysisNoClosureIn: class for: ast
	"Look up vars in classOrScope.  My tree will be annotated with bindings to Scopes and Variables."

	OCASTSemanticAnalyzer new
		compilationContext: class compiler compilationContext;
		visitNode: ast.