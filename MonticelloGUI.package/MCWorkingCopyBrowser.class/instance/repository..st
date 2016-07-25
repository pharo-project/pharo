actions
repository: aRepository
	repository := aRepository.
	workingCopy ifNotNil: [self defaults at: workingCopy put: aRepository]