accessing-computed
currentVersion: aVersion

	currentVersion  := aVersion.
	branch := baselines := versions := infoList := packages := projects := groups := text := nil.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
	self announcer announce: (MBInfoListChanged changed: self).
