initialization
availablePluginsFor: aRepository
	^ (IcePlugin allAvailableFor: aRepository) 
		collect: #new
		as: Array