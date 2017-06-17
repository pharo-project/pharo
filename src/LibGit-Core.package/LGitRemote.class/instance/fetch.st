operations
fetch
	self isReady ifFalse: [ self create ].

	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: self ffiNull handle
			opts: (LGitFetchOptions defaults
				callbacks: LGitRemoteCallbacks defaults;
				yourself)
			reflog_message: nil ]