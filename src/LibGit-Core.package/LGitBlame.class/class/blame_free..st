libgit-calls
blame_free: handle
	
	^ self call: #(void git_blame_free #(ulong handle)) options: #()