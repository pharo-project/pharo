private
lookup
	self withReturnHandlerDo: [ 
		self
			commit_lookup: self
			repo: self repository
			id: self id ].
	self autoRelease