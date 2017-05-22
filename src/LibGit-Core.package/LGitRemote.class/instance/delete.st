operations
delete
	self withReturnHandlerDo: [
		self
			remote_delete: self repository
			name: self remoteName ]