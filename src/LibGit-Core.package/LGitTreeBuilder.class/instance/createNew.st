operations
createNew
	self withReturnHandlerDo: [ 
		self 
			treebuilder_new: self
			repo: self repository
			source: self ffiNull handle ]