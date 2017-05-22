libgit-calls
credentials: credentials
	
	^ self
		call: #(FFIVoid #(LGitCredentials * credentials))
		options: #(optMayGC)