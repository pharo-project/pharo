visitor
visitMethodAddition: aMethodCreatedChange
	^ self 
		printProtocol: aMethodCreatedChange protocol
		sourceCode: aMethodCreatedChange sourceCode