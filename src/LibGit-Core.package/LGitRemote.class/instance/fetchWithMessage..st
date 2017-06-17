operations
fetchWithMessage: aString
	self create.
		
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: LGitFetchOptions defaults
			reflog_message: aString ]