operations
hideCommit: aLGitId
	self withReturnHandlerDo: [ self revwalk_hide: aLGitId ]