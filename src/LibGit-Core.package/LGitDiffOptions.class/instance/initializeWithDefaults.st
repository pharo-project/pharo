operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			diff_init_options: self
			version: LGitOptionsVersionsEnum git_diff_options_version_1 ]