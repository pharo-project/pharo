operations
iterate: aLGitBranchTypeEnum
	self repository signalIfNotReady.
	self withReturnHandlerDo: [
		self
			branch_iterator_new: self
			repo: self repository
			list_flags: aLGitBranchTypeEnum ]