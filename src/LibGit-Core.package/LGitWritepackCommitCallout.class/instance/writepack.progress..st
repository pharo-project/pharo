libgit-calls
writepack: writepack progress: progress
	
	^ self
		call:
			#(FFIInt32 #(LGitOdbWritepack * writepack , LGitTransferProgress * progress))
		options: #(optMayGC)