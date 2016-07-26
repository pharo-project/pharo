evaluation
recompileAST
	| links |
	OCASTSemanticCleaner clean: ast.
	ast compilationContext 
		semanticAnalyzerClass: RFSemanticAnalyzer;
		astTranslatorClass: RFASTTranslator.
	ast doSemanticAnalysis. "force semantic analysis"
	compiledMethod := ast generate: compiledMethod trailer.
	compiledMethod reflectiveMethod: self.