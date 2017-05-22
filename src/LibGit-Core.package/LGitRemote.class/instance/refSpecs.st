accessing
refSpecs 
	| specs |
	specs := LGitStringArray externalNew autoRelease.
	self withReturnHandlerDo: [ 
		self remote_get_fetch_refspecs: specs ].
	^ specs strings