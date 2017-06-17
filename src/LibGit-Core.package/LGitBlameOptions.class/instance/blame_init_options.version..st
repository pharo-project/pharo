libgit-calls
blame_init_options: opts version: version
	^ self
		callUnchecked: #(FFIInt32 git_blame_init_options(LGitBlameOptions * self, LGitOptionsVersionsEnum version))
		options: #( )