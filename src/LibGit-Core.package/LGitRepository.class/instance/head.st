accessing
head
	| head |
	head := LGitReference of: self.
	self withReturnHandlerDo: [ self repository_head: head of: self ].
	^ head