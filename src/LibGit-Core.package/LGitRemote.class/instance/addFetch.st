operations
addFetch
	self withReturnHandlerDo: [ 
		self
			remote_add_fetch: self repository
			remote: self remoteName
			refspec: self fetchSpec ]