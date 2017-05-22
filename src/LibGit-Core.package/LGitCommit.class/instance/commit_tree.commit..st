libgit-calls
commit_tree: tree_out commit: commit
	
	^ self
		call: #(LGitReturnCodeEnum git_commit_tree #(LGitTree * tree_out , self))
		options: #()