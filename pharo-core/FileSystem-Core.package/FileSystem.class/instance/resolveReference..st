navigating
resolveReference: aReference
	^ aReference fileSystem = self ifTrue:
		[workingDirectory resolvePath: aReference path]