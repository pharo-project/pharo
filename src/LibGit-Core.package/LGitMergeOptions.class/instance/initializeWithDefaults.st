operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			merge_init_options: self
			version: LGitOptionsVersionsEnum git_merge_options_version_1 ]