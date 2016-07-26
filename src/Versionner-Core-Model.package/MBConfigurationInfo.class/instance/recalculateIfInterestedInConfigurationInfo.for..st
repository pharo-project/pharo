updating
recalculateIfInterestedInConfigurationInfo: configInfo for: operation

	projects ifNil: [ ^self ].
	self projects detect: [ :info | info interestedInConfigurationInfo: configInfo ] ifNone: [ ^self ].
	infoList := text := projects := nil.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self)