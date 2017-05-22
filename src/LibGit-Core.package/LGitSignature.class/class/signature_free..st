libgit-calls
signature_free: handle 
	^ self
		call: #(void git_signature_free(void *handle))
		options: #(  )