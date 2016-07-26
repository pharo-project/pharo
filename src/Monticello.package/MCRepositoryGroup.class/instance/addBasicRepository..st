adding
addBasicRepository: aRepository
	((repositories includes: aRepository) 
	or: [ aRepository == MCCacheRepository uniqueInstance ])
		ifFalse: [
			repositories add: aRepository.
			self announceRepositoryAdded: aRepository.
			self class default addRepository: aRepository ].
	^ aRepository