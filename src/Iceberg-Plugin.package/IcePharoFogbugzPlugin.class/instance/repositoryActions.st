accessing
repositoryActions
	^ { 
	self newIssueFixAction.
	self newPullRequestAction.
	}