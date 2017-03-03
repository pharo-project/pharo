navigating
resolveReference: aReference
	^ aReference fileSystem = self ifTrue:
		[self workingDirectoryPath resolvePath: aReference path]