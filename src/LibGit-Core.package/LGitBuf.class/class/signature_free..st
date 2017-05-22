libgit-calls
signature_free: handle 
	^ self
		call: #(void git_buf_free(void *handle))
		options: #(  )