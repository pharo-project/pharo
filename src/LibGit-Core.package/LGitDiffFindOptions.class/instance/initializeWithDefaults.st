operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			diff_find_init_options: self
			version: LGitOptionsVersionsEnum git_diff_find_options_version_1 ]