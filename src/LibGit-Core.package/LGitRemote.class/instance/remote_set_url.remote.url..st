libgit-calls
remote_set_url: repo remote: remote url: urlString
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_set_url #(LGitRepository repo , FFIExternalString remote , FFIExternalString urlString))
		options: #()