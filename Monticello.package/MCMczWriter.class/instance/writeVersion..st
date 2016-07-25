visiting
writeVersion: aVersion
	self writeFormat.
	self writePackage: aVersion package.
	self writeVersionInfo: aVersion info.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [:ea | self writeVersionDependency: ea]