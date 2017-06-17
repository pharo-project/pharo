libgit-calls
blame_buffer: out reference: reference buffer: buffer buffer_len: buffer_len
	
	^ self
		callUnchecked:
			#(FFIInt32 git_blame_buffer #(LGitBlame * self , LGitBlame reference , FFIExternalString buffer , FFIUInt32 buffer_len))
		options: #()