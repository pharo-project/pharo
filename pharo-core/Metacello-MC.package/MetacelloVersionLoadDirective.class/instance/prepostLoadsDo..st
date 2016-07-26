enumerating
prepostLoadsDo: aBlock

	self loadDirectives do: [:directive | directive prepostLoadDo: aBlock ].