private
remoteBranch: aName
	^ STON fromString: (self requestRemoteBranch: aName)