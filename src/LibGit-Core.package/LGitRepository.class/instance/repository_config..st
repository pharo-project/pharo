libgit-calls
repository_config: config
	^ self
		call:
			#(LGitReturnCodeEnum git_repository_config(LGitConfig *config, self))
		options: #()