libgit-calls
repository_free: handle
	
	^ self call: #(void git_repository_free #(void *handle)) options: #()