actions
removeBranches: aRepository remote: aRemote
	(IceGitHubRemoveBranchesCommand 
		repository: aRepository 
		remote: aRemote)
		execute