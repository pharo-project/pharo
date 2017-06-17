libgit-calls
fetch_init_options: opts version: version
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_fetch_init_options(LGitFetchOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )