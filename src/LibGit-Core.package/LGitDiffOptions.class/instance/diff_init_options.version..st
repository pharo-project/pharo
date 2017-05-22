libgit-calls
diff_init_options: opts version: version
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_diff_init_options(LGitDiffOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )