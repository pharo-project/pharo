operations
setHeadDetached: commitish
	self withReturnHandlerDo: [ 
		self repository_set_head_detached: commitish id
	]