libgit-calls
commit_time_offset: commit
	
	^ self call: #(int git_commit_time_offset #(self)) options: #()