enumerating
allAvailableDependenciesDo: aBlock
	
	self dependencies do:
		[:ea |
		[ | version |version := ea resolve.
		version allAvailableDependenciesDo: aBlock.
		aBlock value: version]
			on: Error do: []]