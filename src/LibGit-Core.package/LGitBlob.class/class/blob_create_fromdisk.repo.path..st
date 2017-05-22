libgit-calls
blob_create_fromdisk: objectId repo: repo path: path
	
	^ self
		call:
			#(LGitReturnCodeEnum git_blob_create_fromdisk #(LGitId * objectId , LGitRepository repo , FFIExternalString path))
		options: #()