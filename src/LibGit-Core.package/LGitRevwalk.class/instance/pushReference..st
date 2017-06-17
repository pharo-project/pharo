operations
pushReference: aReference
	self withReturnHandlerDo: [
		self
			revwalk_push_ref: self
			refname: aReference name ]