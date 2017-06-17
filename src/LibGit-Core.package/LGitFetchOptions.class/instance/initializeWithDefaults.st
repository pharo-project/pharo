operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			fetch_init_options: self
			version: LGitOptionsVersionsEnum git_fetch_options_version_1 ]