asserting
denyAListHasSelection: aString
	| found |
	found := true.
	self listMorphs 
			detect: [:m | m selection = aString]
			ifNone: [found := false].
	self deny: found.