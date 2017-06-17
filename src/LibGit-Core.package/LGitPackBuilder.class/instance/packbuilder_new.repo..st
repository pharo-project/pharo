libgit-calls
packbuilder_new: out repo: repo
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_packbuilder_new #(LGitPackBuilder * self , LGitRepository repo))
		options: #()