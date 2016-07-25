actions
loadRepositories
	FileStream fileIn: 'MCRepositories.st'.
	self repositoryListChanged.
	self changedButtons.
