morphic ui
workingCopySelectionWrapper: aWrapper
	workingCopyWrapper := aWrapper.
	self changed: #workingCopySelectionWrapper.
	self workingCopy: (aWrapper ifNotNil:[aWrapper item])