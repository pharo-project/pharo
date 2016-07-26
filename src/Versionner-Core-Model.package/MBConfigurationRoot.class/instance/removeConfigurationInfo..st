actions
removeConfigurationInfo: aConfigurationInfo

	aConfigurationInfo release.
	self configurations remove: aConfigurationInfo ifAbsent: [].
	self announcer announce: MBConfigurationInfoDeleted