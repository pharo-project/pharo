operations
lookup
	self withReturnHandlerDo: [ 
		self
			remote_lookup: self
			repo: self repository
			name: self remoteName.
		self url: self remote_url ].
	self flag: #todo. "shouldn't be autoreleased?"