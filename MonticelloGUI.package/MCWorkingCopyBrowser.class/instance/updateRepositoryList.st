updating
updateRepositoryList
	self repositoryListChanged.
	self changed: #repositorySelection.
	self changedButtons.