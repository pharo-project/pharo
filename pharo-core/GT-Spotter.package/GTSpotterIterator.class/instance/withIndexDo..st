mimic enumerating
withIndexDo: aBlock
	| index |
	index := 0.
	self do: [ :each | aBlock cull: each cull: (index := index + 1) ]