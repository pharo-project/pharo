libgit-calls
index_write
	^ self
		call: #(LGitReturnCodeEnum git_index_write #(self))
		options: #()