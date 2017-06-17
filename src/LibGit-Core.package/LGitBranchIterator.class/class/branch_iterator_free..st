libgit-calls
branch_iterator_free: handle
	
	^ self
		call: #(void git_branch_iterator_free #(void *handle))
		options: #()