libgit-calls
remote_lookup: out repo: repo name: name
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_remote_lookup #(LGitRemote * self , LGitRepository repo , String name))
		options: #()