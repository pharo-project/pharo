asserting
denyAListIncludesAnyOf: anArrayOfStrings
	| found |
	found := true.
	self listMorphs 
			detect: [:m | m getList includesAnyOf: anArrayOfStrings]
			ifNone: [found := false].
	self deny: found.