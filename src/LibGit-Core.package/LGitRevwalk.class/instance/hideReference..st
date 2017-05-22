operations
hideReference: aReference
	self withReturnHandlerDo: [
		self
			revwalk_hide_ref: self
			refname: aReference name ]