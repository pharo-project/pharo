libgit-calls
blob_rawsize: blob
	
	^ self call: #(FFIInt64 git_blob_rawsize #(self)) options: #()