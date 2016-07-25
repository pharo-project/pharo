morphic ui
workingCopy: wc
	workingCopy := wc.
	self changed: #workingCopySelection; repositoryListChanged.
	self changedButtons.
