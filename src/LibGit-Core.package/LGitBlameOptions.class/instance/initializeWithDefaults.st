operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			blame_init_options: self
			version: LGitOptionsVersionsEnum git_blame_options_version_1 ]