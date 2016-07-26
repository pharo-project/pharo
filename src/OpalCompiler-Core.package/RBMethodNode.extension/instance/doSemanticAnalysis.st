*OpalCompiler-Core
doSemanticAnalysis

	self compilationContext ifNil: [ self methodClass: nil class ].

	self
		compilationContext semanticAnalyzerClass new
		compilationContext: self compilationContext;
		analyze: self