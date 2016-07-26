visitor
visitMethodModification: aMethodModification

	^ self 
		printProtocol: aMethodModification oldProtocol  
		sourceCode: aMethodModification oldSourceCode