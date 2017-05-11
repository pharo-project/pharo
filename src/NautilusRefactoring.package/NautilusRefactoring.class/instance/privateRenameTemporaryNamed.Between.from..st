private-source
privateRenameTemporaryNamed: oldname Between: anInterval from: aMethod

	^ RBRenameTemporaryRefactoring
		model: environment
		renameTemporaryFrom: anInterval
		to: (self request: self newVariableRequestText initialAnswer: oldname)
		in: aMethod methodClass
		selector: aMethod selector