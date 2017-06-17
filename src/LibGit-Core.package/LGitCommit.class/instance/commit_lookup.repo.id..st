libgit-calls
commit_lookup: commit repo: repo id: objectId
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_commit_lookup #(LGitCommit * commit , LGitRepository repo , LGitId * objectId))
		options: #()