initialization
initializeRepository: aRepository.
	self initialize.
	plugins := self availablePluginsFor: aRepository