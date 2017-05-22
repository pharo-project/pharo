lookup
lookupBranch: name type: branchType
	| reference |
	reference := LGitReference of: self.
	self withReturnHandlerDo: [ 
		self
			branch_lookup: reference
			branch_name: name
			branch_type: branchType
	].
	^ reference
