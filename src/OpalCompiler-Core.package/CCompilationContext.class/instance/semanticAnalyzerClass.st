accessing
semanticAnalyzerClass
	^ semanticAnalyzerClass ifNil: [ semanticAnalyzerClass := OCASTSemanticAnalyzer  ]