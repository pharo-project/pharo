libgit-calls
repository_set_head: refname log_message: log_message
	
	^ self
		call:
			#(int git_repository_set_head #(self , String refname , String log_message))
		options: #()