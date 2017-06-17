libgit-calls
revwalk_push_head: walk
	
	^ self
		call: #(LGitReturnCodeEnum git_revwalk_push_head #(self))
		options: #()