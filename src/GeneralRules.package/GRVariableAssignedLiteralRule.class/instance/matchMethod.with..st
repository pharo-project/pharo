running-helpers
matchMethod: aCompiledMethod with: aVarName

	^ ((self matcherFor: aVarName) 
		executeTree: aCompiledMethod ast
		initialAnswer: nil) isNotNil 