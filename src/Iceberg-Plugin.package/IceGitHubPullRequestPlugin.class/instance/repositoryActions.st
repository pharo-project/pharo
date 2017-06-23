accessing
repositoryActions
	^ { 
	self newPullRequestAction. 
	self removeOldBranchesAction. 
	}