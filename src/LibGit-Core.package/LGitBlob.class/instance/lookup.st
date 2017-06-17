private
lookup
	self withReturnHandlerDo: [ self
		blob_lookup: self 
		repo: self repository
		id: self id ].
	self autoRelease