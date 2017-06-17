libgit-calls
branch_set_upstream: upstream
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_set_upstream #(self , String upstream))
		options: #()