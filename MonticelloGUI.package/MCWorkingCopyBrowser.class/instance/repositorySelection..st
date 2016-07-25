morphic ui
repositorySelection: aNumber
	aNumber = 0
		ifTrue: [self repository: nil]
		ifFalse: [self repository: (self repositories at: aNumber)].
	self changed: #repositorySelection.
	self changedButtons.
