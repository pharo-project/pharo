visitor
visitMethodRemoval: aMethodRemoval

	^ self 
		printProtocol: aMethodRemoval protocol
		sourceCode: aMethodRemoval sourceCode