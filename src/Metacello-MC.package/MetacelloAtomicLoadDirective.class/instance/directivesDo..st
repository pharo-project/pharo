enumerating
directivesDo: aBlock

	aBlock value: self.
	self preloads do: [:directive | directive directivesDo: aBlock ].
	self loadDirectives do: [:directive | directive directivesDo: aBlock ].
	self postloads do: [:directive | directive directivesDo: aBlock ].