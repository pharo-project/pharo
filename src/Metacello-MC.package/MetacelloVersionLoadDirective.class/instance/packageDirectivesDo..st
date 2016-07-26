enumerating
packageDirectivesDo: aBlock

	self loadDirectives do: [:directive | directive packageDirectivesDo: aBlock ].