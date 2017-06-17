operations
open
	self withReturnHandlerDo: [ 
		self
			repository_open: self
			bare_path: self repositoryReference pathString ].
	self beOpen