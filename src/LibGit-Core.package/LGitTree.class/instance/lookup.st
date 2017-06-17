private
lookup
	self withReturnHandlerDo: [
		self
			tree_lookup: self
			repo: self repository
			id: self id ].
	self autoRelease