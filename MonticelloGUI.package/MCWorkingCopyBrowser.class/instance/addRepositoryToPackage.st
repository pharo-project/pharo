actions
addRepositoryToPackage
	self repository ifNotNil:
		[:repos |
		(self pickWorkingCopySatisfying: [ :p | (p repositoryGroup includes: repos) not ]) ifNotNil:
			[:wc |
			workingCopy := wc.
			workingCopy repositoryGroup addRepository: repos.
			self repository: repos.	
			self
				changed: #workingCopySelection;
				repositoryListChanged;
				changed: #repositorySelection.
			self changedButtons]]