visitor
visitMethodModification: aMethodModification

	^ self 
		printProtocol: aMethodModification newProtocol
		sourceCode: aMethodModification newSourceCode 