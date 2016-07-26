*OpalCompiler-Core
generateIR
	| ir |
	scope ifNil: [self doSemanticAnalysisInContext: self compilationContext ].
 	ir := (self compilationContext astTranslatorClass new
			visitNode: self)
			ir.
	^ self ir: ir