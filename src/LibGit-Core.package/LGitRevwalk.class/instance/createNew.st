private
createNew
	self withReturnHandlerDo: [
		self
			revwalk_new: self
			repo: self repository ]