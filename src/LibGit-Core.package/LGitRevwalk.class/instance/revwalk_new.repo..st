libgit-calls
revwalk_new: out repo: repo
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_revwalk_new #(LGitRevwalk * self , LGitRepository repo))
		options: #()