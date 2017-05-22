libgit-calls
blob_is_binary: blob
	
	^ self call: #(Boolean git_blob_is_binary #(self)) options: #()