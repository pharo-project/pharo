accessing-metadata
setNoMetadata
	self hasMetadata
		ifTrue:
			[ self repositoryProperties at: 'Metadata' put: 'false'.
			self writeRepositoryProperties ]
