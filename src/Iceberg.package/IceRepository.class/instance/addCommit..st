private
addCommit: newCommit
	newCommit repository: self.
	self commitDictionary 
		at: newCommit id 
		put: newCommit
