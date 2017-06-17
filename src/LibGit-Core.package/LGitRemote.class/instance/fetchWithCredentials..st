operations
fetchWithCredentials: aCredentialsProvider
	self isReady ifFalse: [ self create ].

	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: self ffiNull handle
			opts: (LGitFetchOptions defaults
				callbacks:(LGitRemoteCallbacks withProvider: aCredentialsProvider);
				yourself)
			reflog_message: nil ]