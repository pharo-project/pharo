morphic ui
workingCopySelection: aNumber
	self workingCopy: 
		(aNumber = 0 
			ifTrue: [nil]
			ifFalse: [self workingCopies at: aNumber]).	