operations
database
	| database |
	database := LGitOdb new.
	self withReturnHandlerDo: [
		self
			repository_odb: database
			repo: self ].
		
	^ database