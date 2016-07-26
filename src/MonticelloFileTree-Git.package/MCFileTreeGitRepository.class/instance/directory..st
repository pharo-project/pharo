accessing
directory: aDirectory
	(self isGitRepository: aDirectory)
		ifFalse: [ MCFileTreeGitError new
				signal: 'is not a git repository: ' , aDirectory ].
	super directory: aDirectory