libgit-calls
index_write_tree: out index: index
	
	^ self
		call: #(LGitReturnCodeEnum git_index_write_tree #(LGitId * out , self))
		options: #()