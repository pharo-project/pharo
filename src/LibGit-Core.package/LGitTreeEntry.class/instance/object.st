accessing
object
	| object |
	object := (LGitObject classFor: self type) of: self repository.
	self withReturnHandlerDo: [
		self
			tree_entry_to_object: object
			repo: self repository
			entry: self ].
		
	^ object