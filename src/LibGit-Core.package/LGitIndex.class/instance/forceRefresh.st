operations
forceRefresh
	self withReturnHandlerDo: [ 
		self
			index_read: self
			force: true ]