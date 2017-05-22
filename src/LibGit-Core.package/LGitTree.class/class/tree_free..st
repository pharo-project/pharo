libgit-calls
tree_free: handle
	
	^ self call: #(void git_tree_free #(void *handle)) options: #()