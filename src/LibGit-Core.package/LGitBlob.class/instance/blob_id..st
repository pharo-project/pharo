libgit-calls
blob_id: blob
	
	^ self call: #(void * git_blob_id #(self)) options: #()