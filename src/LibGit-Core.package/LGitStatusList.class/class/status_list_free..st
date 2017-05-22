libgit-calls
status_list_free: handle
	
	^ self
		call: #(void git_status_list_free #(void *handle))
		options: #()