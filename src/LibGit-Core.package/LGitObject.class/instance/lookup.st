private
lookup
	self withReturnHandlerDo: [ self
		object_lookup: self 
		repo: self repository
		id: self id 
		type: LGitObjectTypeEnum git_obj_any ]