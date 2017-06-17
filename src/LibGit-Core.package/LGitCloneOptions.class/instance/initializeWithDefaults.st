operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			clone_init_options: self
			version: LGitOptionsVersionsEnum  git_clone_options_version_1 ]