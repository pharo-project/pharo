operations
initBare: aBoolean
	self withReturnHandlerDo: [
		self
			repository_init: self
			path: self repositoryReference pathString
			is_bare: aBoolean ].
	self autoRelease 