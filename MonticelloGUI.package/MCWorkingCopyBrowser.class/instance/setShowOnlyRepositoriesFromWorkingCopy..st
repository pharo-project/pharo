actions
setShowOnlyRepositoriesFromWorkingCopy: anIndex
	showOnlyRepositoriesFromWorkingCopy  := (anIndex = 2).
	self repositorySelection: 0.
	self repositoryListChanged.
