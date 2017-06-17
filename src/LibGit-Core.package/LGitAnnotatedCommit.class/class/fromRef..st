instance creation
fromRef: anLGitReference
	^ (self of: anLGitReference repository) initializeFromRef: anLGitReference 