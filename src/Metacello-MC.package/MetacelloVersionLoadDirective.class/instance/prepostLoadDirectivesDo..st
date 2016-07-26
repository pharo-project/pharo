enumerating
prepostLoadDirectivesDo: aBlock

	self loadDirectives do: [:directive | directive prepostLoadDirectivesDo: aBlock ].