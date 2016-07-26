enumerating
versionDirectivesDo: aBlock

	aBlock value: self.
	self loadDirectives do: [:directive | directive versionDirectivesDo: aBlock ].