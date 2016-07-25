accessing
removeRepository: aRepository
	repositories remove: aRepository ifAbsent: [ ^ self ].
	self announceRepositoryRemoved: aRepository