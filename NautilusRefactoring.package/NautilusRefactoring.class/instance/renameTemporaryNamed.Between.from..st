source
renameTemporaryNamed: oldname Between: anInterval from: aMethod
	aMethod ifNil: [ ^ self ].
	self performRefactoringFor: #privateRenameTemporaryNamed:Between:from: withArguments: {oldname. anInterval. aMethod}.