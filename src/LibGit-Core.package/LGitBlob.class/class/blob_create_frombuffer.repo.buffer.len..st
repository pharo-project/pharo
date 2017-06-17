libgit-calls
blob_create_frombuffer: objectId repo: repo buffer: buffer len: len
	
	^ self
		call:
			#(LGitReturnCodeEnum git_blob_create_frombuffer #(LGitId * objectId , LGitRepository repo , FFIVoid * buffer , FFIInt32 len))
		options: #()