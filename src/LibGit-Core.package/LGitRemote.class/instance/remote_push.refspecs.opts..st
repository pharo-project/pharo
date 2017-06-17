libgit-calls
remote_push: remote refspecs: refspecs opts: opts
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_push #(self , LGitStringArray * refspecs , LGitPushOptions * opts))
		options: #(optCoerceNilToNull)