operations
initializeWithDefaults
	self withReturnHandlerDo: [
		self
			checkout_init_options: self
			version: LGitOptionsVersionsEnum git_checkout_options_version_1 ]