initialize
initializeFromRef: anLGitReference
	self withReturnHandlerDo: [ 
		self annotated_commit_from_ref: anLGitReference
	].

	self autoRelease