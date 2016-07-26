accessing-metadata
hasMetadata
	^ ((self repositoryProperties at: 'Metadata' ifAbsent: [  ])
		= 'false') not