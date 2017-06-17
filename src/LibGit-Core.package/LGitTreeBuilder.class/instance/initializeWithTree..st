initialization
initializeWithTree: aTree
	self withReturnHandlerDo: [
		self
			treebuilder_new: self
			repo: self repository
			source: (aTree ifNotNil: #handle) ]