actions
load
	self hasVersion
		ifFalse: [ ^ self ].
	super load.
	self version workingCopy repositoryGroup addRepository: repository