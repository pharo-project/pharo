operations
diffIndexToWorkDirOptions: anLGitDiffOptions
	self withReturnHandlerDo: [
		self
			diff_index_to_workdir: self
			repo: self repository
			index: self repository index
			opts: anLGitDiffOptions ]