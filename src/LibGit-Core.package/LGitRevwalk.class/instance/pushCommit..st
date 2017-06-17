operations
pushCommit: aLGitId 
	self withReturnHandlerDo: [ self revwalk_push: aLGitId ]