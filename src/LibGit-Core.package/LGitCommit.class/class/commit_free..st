libgit-calls
commit_free: handle
	
	^ self call: #(void git_commit_free #(void *handle)) options: #()