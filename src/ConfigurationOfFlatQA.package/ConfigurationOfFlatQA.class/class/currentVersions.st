mc-manipulation
currentVersions

	^ self rootConfNamesForLookup flatCollect: [ :className |
		self currentVersionsOf: className asClass ]
