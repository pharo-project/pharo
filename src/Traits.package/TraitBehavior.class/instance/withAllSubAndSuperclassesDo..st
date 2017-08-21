user interface
withAllSubAndSuperclassesDo: aBlock

	self withAllSubclassesDo: aBlock.
	self allSuperclassesDo: aBlock.
