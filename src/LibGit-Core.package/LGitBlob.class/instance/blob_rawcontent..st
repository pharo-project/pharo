libgit-calls
blob_rawcontent: blob
	
	^ self call: #(void * git_blob_rawcontent #(self)) options: #()