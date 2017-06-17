libgit-calls
reference_lookup: out repo: repo name: name
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_lookup #(LGitReference * self , LGitRepository repo , String name))
		options: #()