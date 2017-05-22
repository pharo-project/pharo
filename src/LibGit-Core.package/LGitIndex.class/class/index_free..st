libgit-calls
index_free: handle
	
	^ self call: #(void git_index_free #(void *handle)) options: #()