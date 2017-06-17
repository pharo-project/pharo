actions
newPullRequest: aRepository
	(IceGitHubNewPullRequestCommand repository: aRepository) 
		remote: aRepository pushRemote;
		execute