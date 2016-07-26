actions
addRepository: aRepository
	self repository: aRepository.
	self repositoryGroup addRepository: aRepository.
	self updateRepositoryList.