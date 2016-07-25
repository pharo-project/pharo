actions
load
	self hasVersion
		ifFalse: [ ^ self ].
	self version isCacheable
		ifTrue: [ version workingCopy repositoryGroup addRepository: repository ].
	super load.
	self invalidatePackageAncestryCache.
	self refresh