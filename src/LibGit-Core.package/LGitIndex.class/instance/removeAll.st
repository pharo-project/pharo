operations
removeAll
	| pathspec |
	pathspec := LGitStringArray withAll: #().
	self withReturnHandlerDo: [ 
		self
			index_remove_all: self
			pathspec: pathspec
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: nil ]