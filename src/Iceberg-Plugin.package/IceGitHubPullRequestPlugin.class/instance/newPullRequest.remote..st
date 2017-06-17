actions
newPullRequest: aRepository remote: aRemote
	(IceGitHubNewPullRequestCommand repository: aRepository) 
		remote: aRemote;
		execute