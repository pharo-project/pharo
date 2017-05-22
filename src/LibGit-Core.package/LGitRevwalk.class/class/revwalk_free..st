libgit-calls
revwalk_free: handle
	
	^ self call: #(void git_revwalk_free #(void *handle)) options: #()