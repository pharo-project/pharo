libgit-calls
index_read: index force: force
	
	^ self
		call: #(LGitReturnCodeEnum git_index_read #(self , Boolean force))
		options: #()