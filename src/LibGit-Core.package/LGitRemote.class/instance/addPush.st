operations
addPush
	self withReturnHandlerDo: [ 
		self
			remote_add_push: self repository
			remote: self remoteName
			refspec: self fetchSpec ]