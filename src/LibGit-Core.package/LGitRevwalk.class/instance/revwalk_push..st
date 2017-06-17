libgit-calls
revwalk_push: commitId
	^ self 
		call:
			#(LGitReturnCodeEnum git_revwalk_push #(self , LGitId * commitId))
		options: #()