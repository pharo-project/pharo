libgit-calls
remote_init_callbacks: opts version: version 
	^ self
		call: #(LGitReturnCodeEnum git_remote_init_callbacks(LGitRemoteCallbacks * self, LGitStructVersionsEnum version))
		options: #(  )