libgit-calls
tag_list: tag_names repo: repo
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tag_list #(LGitStringArray * tag_names , LGitRepository repo))
		options: #()