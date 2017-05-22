operations
pullWithFetchOptions: fetchOptions
	<todo: 'check for conflicts'>
	self repository isReady ifFalse: [  ^ self ].

	self lookup.
	
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: fetchOptions
			reflog_message: 'automatic pull from libgit2' ].
	
	"assume that there are no conflicts and fast-forwad to FETCH_HEAD"
	self repository fastForward