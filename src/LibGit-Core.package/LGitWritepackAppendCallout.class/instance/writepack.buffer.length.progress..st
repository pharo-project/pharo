libgit-calls
writepack: writepack buffer: buffer length: length progress: progress
	
	^ self
		call:
			#(FFIInt32 #(LGitOdbWritepack * writepack , FFIVoid * buffer , FFIInt32 length , LGitTransferProgress * progress))
		options: #(optMayGC)