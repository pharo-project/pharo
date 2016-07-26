accessing
at: aCompiledMethod
	^ self 
		at: aCompiledMethod
		ifAbsentPut: [ 
			aCompiledMethod parseTree doSemanticAnalysisIn: aCompiledMethod methodClass ]