libgit-calls
commit_message_encoding: commit
	
	^ self
		call: #(String git_commit_message_encoding #(self))
		options: #()