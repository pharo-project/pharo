refactory-changes-methods
addMethodSource: sourceCode in: aClass classified: aProtocol for: aController
	^ RBAddMethodChange
		compile: sourceCode
		in: aClass
		classified: aProtocol
		for: aController