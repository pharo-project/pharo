accessing-metadata
setWithMetadata
	self hasMetadata
		ifFalse:
			[ self repositoryProperties removeKey: 'Metadata'.
			self writeRepositoryProperties ]
