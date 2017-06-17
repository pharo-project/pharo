walk definition
fromTag: tag
	self revwalk.
	^ self fromLGitId: (lgitRepository revparse: tag name) id

