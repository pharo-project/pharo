libgit-calls
odb_free: handle
	
	^ self call: #(void git_odb_free #(void *handle)) options: #()