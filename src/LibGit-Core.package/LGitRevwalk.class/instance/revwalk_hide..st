libgit-calls
revwalk_hide: commitId
	^ self 
		call:
			#(LGitReturnCodeEnum git_revwalk_hide #(self , LGitId * commitId))
		options: #()