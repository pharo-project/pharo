operations
clone: aString
	| cloneOptions |
	cloneOptions := LGitCloneOptions defaults.
	self
		clone: aString
		options: cloneOptions	
