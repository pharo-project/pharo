libgit-calls
remote_free: handle
	
	^ self call: #(void git_remote_free #(void *handle)) options: #()