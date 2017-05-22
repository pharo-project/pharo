libgit-calls
reference_free: handle
	^ self
		call: #(void git_reference_free #(void *handle))
		options: #()