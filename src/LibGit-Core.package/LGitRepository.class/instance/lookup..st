lookup
lookup: name
	| reference |
	reference := LGitReference of: self.
	self withReturnHandlerDo: [
		self
			reference_lookup: reference
			repo: self
			shorthand: name ].
		
	^ reference