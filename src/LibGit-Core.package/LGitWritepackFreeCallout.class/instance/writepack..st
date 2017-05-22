libgit-calls
writepack: writepack
	
	^ self call: #(FFIVoid #(LGitOdbWritepack * writepack)) options: #()