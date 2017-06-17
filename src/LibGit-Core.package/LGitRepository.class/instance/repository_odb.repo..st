libgit-calls
repository_odb: out repo: repo
	
	^ self
		call: #(LGitReturnCodeEnum git_repository_odb #(LGitOdb * out , self))
		options: #()