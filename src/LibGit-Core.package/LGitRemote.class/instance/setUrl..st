operations
setUrl: aString
	self withReturnHandlerDo: [ 
		self
			remote_set_url: self repository
			remote: self remoteName
			url: aString]