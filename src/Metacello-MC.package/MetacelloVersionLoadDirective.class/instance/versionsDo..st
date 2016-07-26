enumerating
versionsDo: aBlock

	self loadDirectives do: [:directive | directive versionDo: aBlock ].