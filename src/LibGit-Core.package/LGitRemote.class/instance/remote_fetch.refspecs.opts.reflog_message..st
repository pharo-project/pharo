libgit-calls
remote_fetch: remote refspecs: refspecs opts: opts reflog_message: reflog_message
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_fetch #(self , void * refspecs , LGitFetchOptions * opts , String reflog_message))
		options: #(optCoerceNilToNull)