libgit-calls
index_has_conflicts 
	^ self call: #(LGitReturnCodeEnum git_index_has_conflicts #(self)) options: #()