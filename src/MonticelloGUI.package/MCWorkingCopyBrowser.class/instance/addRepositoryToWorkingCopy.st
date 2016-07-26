actions
addRepositoryToWorkingCopy
	workingCopy ifNotNil:
		[:wc |
			workingCopy repositoryGroup addRepository: self repository.
			self
				changed: #workingCopySelection;
				repositoryListChanged;
				changed: #repositorySelection.
			self changedButtons]