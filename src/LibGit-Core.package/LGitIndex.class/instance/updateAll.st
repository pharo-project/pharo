operations
updateAll
	| pathspec |
	pathspec := LGitStringArray withAll: { '*' }.
	self withReturnHandlerDo: [ 
		self
			index_update_all: self
			pathspec: pathspec
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: nil ]