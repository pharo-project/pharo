libgit-calls
repository_index: out repo: repo
	
	^ self
		call: #(LGitReturnCodeEnum git_repository_index #(LGitIndex * out , self))
		options: #()