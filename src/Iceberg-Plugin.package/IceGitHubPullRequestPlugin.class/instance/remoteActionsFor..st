accessing
remoteActionsFor: aRemote
	^ { 
	self newPullRequestRemoteAction.
	self removeOldBranchesRemoteAction. 
	}