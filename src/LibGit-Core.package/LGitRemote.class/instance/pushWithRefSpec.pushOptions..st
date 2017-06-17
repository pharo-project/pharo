operations
pushWithRefSpec: aRefSpec pushOptions: pushOptions
	| refspecs return |
	refspecs := LGitStringArray withAll: { aRefSpec asString }.
	return := self withReturnHandlerDo: [
		self
			remote_push: self
			refspecs: refspecs
			opts: pushOptions ].
		
	^ return