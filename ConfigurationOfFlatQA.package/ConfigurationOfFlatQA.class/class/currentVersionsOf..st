mc-manipulation
currentVersionsOf: aConf

	^ aConf project versions first packages collect: [ :p |
		self versionFromWC: (MCPackage named: p name) workingCopy ]