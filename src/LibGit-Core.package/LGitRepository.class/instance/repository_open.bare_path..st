libgit-calls
repository_open: out bare_path: bare_path
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_repository_open #(LGitRepository * out , String bare_path))
		options: #()