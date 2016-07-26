comparing
containsReference: aReference
	^  aReference fileSystem = filesystem and: [path contains: aReference path]