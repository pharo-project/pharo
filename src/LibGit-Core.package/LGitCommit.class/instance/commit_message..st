libgit-calls
commit_message: commit
	
	^ self call: #(String git_commit_message #(self)) options: #()