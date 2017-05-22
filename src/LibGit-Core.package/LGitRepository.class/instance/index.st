accessing
index
	| index |
	index := LGitIndex of: self.
	self withReturnHandlerDo: [ self repository_index: index repo: self ].
	^ index