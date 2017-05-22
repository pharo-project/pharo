libgit-calls
remote_add_push: repo remote: remote refspec: refspec
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_add_push #(LGitRepository repo , FFIExternalString remote , FFIExternalString refspec))
		options: #()