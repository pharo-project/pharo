libgit-calls
packbuilder_free: handle
	
	^ self
		call: #(void git_packbuilder_free #(void *handle))
		options: #()