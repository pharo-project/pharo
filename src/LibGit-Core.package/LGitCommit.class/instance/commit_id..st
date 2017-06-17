libgit-calls
commit_id: commit
	
	^ self call: #(void * git_commit_id #(self)) options: #()