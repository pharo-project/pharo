refactory-changes-methods
addMethodSource: sourceCode in: aClass for: aController
	^ RBAddMethodChange
			compile: sourceCode in: aClass for: aController