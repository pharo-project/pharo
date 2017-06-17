libgit-calls
treebuilder_free: handle
	
	^ self
		call: #(void git_treebuilder_free #(void *handle))
		options: #()