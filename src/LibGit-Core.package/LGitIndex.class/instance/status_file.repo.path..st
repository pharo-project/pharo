libgit-calls
status_file: status_flags repo: repo path: path
	
	^ self
		call:
			#(LGitReturnCodeEnum git_status_file #(LGitWriteBackValueInt status_flags , LGitRepository repo , String path))
		options: #()