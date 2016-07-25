enumerating
prepostLoadsDo: aBlock

	self preloads do: [:directive | directive prepostLoadDo: aBlock ].
	self loadDirectives do: [:directive | directive prepostLoadDo: aBlock ].
	self postloads do: [:directive | directive prepostLoadDo: aBlock ].