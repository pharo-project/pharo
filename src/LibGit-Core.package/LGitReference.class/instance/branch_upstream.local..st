libgit-calls
branch_upstream: out local: local
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_upstream #(LGitReference * out , self))
		options: #()