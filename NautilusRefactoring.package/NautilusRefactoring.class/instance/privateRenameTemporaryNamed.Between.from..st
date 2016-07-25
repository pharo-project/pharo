private-source
privateRenameTemporaryNamed: oldname Between: anInterval from: aMethod
	^ RBRenameTemporaryRefactoring
		model: environment
		renameTemporaryFrom: anInterval
		to: (self request: 'Enter the new variable name:' initialAnswer: oldname)
		in: aMethod methodClass
		selector: aMethod selector