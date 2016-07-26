actions
deleteWorkingCopy
	workingCopy unregister.
	self workingCopySelection: 0.
	self workingCopyListChanged.