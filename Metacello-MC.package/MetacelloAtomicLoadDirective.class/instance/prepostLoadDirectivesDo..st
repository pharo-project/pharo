enumerating
prepostLoadDirectivesDo: aBlock

	self preloads do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
	self loadDirectives do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
	self postloads do: [:directive | directive prepostLoadDirectivesDo: aBlock ].