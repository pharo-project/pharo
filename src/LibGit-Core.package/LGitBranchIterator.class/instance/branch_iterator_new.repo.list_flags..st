libgit-calls
branch_iterator_new: out repo: repo list_flags: list_flags
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_iterator_new #(LGitBranchIterator * self , LGitRepository repo , LGitBranchTypeEnum list_flags))
		options: #()