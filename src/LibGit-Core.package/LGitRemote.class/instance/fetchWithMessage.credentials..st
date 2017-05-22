operations
fetchWithMessage: aString credentials: aCredentialsProvider 
	self create.
		
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: (LGitFetchOptions defaults
				callbacks: (LGitRemoteCallbacks withProvider: aCredentialsProvider);
				yourself)
			reflog_message: aString ]